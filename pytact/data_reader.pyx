# distutils: language = c++
# cython: c_string_type = str
# cython: c_string_encoding = default
# cython: embedsignature = True
# cython: language_level = 3
# distutils: libraries = capnpc capnp capnp-rpc
# distutils: sources = pytact/graph_api.capnp.cpp

"""This module provides read access to dataset files of a Tactican Graph dataset.

The dataset is mapped into memory using mmap, allowing random access to it's
structures while keeping memory low. This file contains three entry-points to a
dataset in order of preference:

1. Contextmanager `data_reader(path)` provides high-level access to the data
   in directory `path`. This is the preferred entry-point unless you need
   something special.
2. Contextmanager `lowlevel_data_reader(path)` provides low-level access to
   the data in directory `path`, giving direct access to the Cap'n Proto structures
   of the dataset. Use this when `data_reader` is too slow or you need access
   to data not provided by `data_reader`.
3. Contextmanager `file_dataset_reader(file)` provides low-level access to
   the Cap'n Proto structures of a single file. Using this is usually not
   advisable.

Additionally, some indexing helpers are defined:
- `GlobalContextSets` calculates and caches the global context of a definition
  as a set, also caching intermediate results.
- `definition_dependencies` and `node_dependencies` traverse the graph starting
  from a node and return all direct, non-transitive definitions that node depends on.
"""

from __future__ import annotations
from contextlib import contextmanager, ExitStack
from dataclasses import dataclass
from typing import Any, Callable, TypeVar, TypeAlias, Union, cast
from collections.abc import Iterable, Sequence, Generator
from pathlib import Path
from immutables import Map
import pytact.graph_api_capnp_cython as apic
import capnp
import pytact.graph_api_capnp as graph_api_capnp
from capnp.includes.types cimport *
from libcpp.vector cimport vector
from libcpp.unordered_set cimport unordered_set
from libcpp.stack cimport stack
from pytact.graph_api_capnp_cython cimport *
import mmap
import itertools

T = TypeVar('T')
class TupleLike():
    def __init__(self, count : int, index_getter : Callable[[int], T]):
        super().__init__()
        self._count = count
        self._index_getter = index_getter
    def __getitem__(self, index: int) -> T:
        if index >= self._count: raise IndexError()
        return self._index_getter(index)
    def __len__(self) -> int:
        return self._count

@contextmanager
def file_dataset_reader(fname: Path) -> Generator[Any, None, None]:
    """Load a single dataset file into memory, and expose it's raw Cap'n Proto structures.

    This is a low-level function. Prefer to use `data_reader` or `lowlevel_data_reader`.
    """
    def create_mmap(fname):
        # No need to keep the file open, at least on linux
        # Closing this prevents file descriptor limits from being exceeded
        with open(fname, 'rb') as f:
            return mmap.mmap(f.fileno(), length=0, access=mmap.ACCESS_READ)
    with create_mmap(fname) as mm:
        with memoryview(mm) as mv:
            with graph_api_capnp.Dataset.from_bytes(
                    mv, traversal_limit_in_words=2**64-1) as g:
                yield apic.Dataset_Reader(g)

cdef struct GraphIndex:
    vector[C_Dataset_Reader] datasets
    vector[C_Graph_Node_Reader_List] nodes
    vector[C_Graph_EdgeTarget_Reader_List] edges
    vector[vector[uint32_t]] local_to_global

cdef class LowlevelDataReader:
    """A thin wrapper around the raw Cap'n Proto structures contained in a dataset directory.

    Every file in the directory is assigned an integer called a graph-id. This class is a
    `Sequence` that allows the retrieval of the structures in a file by its graph-id.
    Additionally, the function `local_to_global` translates a dependency-index relative to
    a graph-id to a new graph-id. This allows one to find out in which file a node in a
    graph is located.

    This is a lowlevel interface. For details on using the exposed structures the documentation
    in the Cap'n Proto API file.
    """

    cdef GraphIndex graph_index

    cdef object graphs # : list[Dataset_Reader]
    cdef readonly object graphid_by_filename # : dict[Path, int]
    """Map from filenames in the data directory to their graph-id's."""

    cdef readonly object graph_files # : list[Path]
    """Map's a graph-id to a filename. Inverse of `graphid_by_filename`."""

    def __cinit__(self, dataset_path : Path, dataset: list[tuple[Path, Dataset_Reader]]):
        """Do not call this initializer directly. Use `lowlevel_data_reader` instead."""
        # Basic, quick sanity check
        if not dataset:
            raise ValueError(f"There does not appear to be a dataset located at {dataset_path}")
        for f, reader in dataset:
            relative_self = Path(reader.dependencies[0])
            if f != relative_self:
                real_root = Path(*(dataset_path/f).parts[:-len(relative_self.parts)])
                raise ValueError(
                    f"Path {dataset_path} doesn't appear to be the root of a dataset. "
                    f"File {dataset_path/f} suggests that the real root might be {real_root}")

        self.graphs = [g for _, g in dataset]
        self.graphid_by_filename = {f: i for i, (f, _) in enumerate(dataset)}
        self.graph_files = [f for f, _ in dataset]

        cdef vector[C_Dataset_Reader] c_datasets
        c_datasets.reserve(len(dataset))
        for _, g in dataset:
            c_datasets.push_back((<Dataset_Reader>g).source)


        cdef vector[C_Graph_Node_Reader_List] c_nodes
        c_nodes.reserve(len(dataset))
        for d in c_datasets:
            c_nodes.push_back(d.getGraph().getNodes())

        cdef vector[C_Graph_EdgeTarget_Reader_List] c_edges
        c_edges.reserve(len(dataset))
        for d in c_datasets:
            c_edges.push_back(d.getGraph().getEdges())

        local_to_global = [[self.graphid_by_filename[Path(f)] for f in g.dependencies] for _, g in dataset]
        self.graph_index = GraphIndex(c_datasets, c_nodes, c_edges, local_to_global)

    def local_to_global(self, graph: int, dep_index: int) -> int:
        """Convert dependency-index relative to a graph-id to a new graph-id.

        This is used to find the graph-id where a particular node can be found. If `graph` is the
        graph-id that contains a reference to a node and `dep_index` is the relative location
        where that node can be found then `local_to_global(graph, dep_index)` finds the graph-id
        of the file where the node is physically located.
        """
        return self.graph_index.local_to_global[graph][dep_index]

    def __getitem__(self, graph):
        """Retrieve the raw Cap'n Proto structures associated with a graph-id."""
        return self.graphs[graph]

    def __len__(self) -> int:
        return len(self.graphs)

@contextmanager
def lowlevel_data_reader(dataset_path: Path) -> Generator[LowlevelDataReader, None, None]:
    """Load a directory of dataset files into memory, and expose their raw Cap'n Proto structures.

    This is a low-level function. Prefer to use `data_reader`. See `LowlevelDataReader` for
    further documentation.
    """
    fnames = [f for f in dataset_path.glob('**/*.bin') if f.is_file()]
    with ExitStack() as stack:
        dataset = [(fname.relative_to(dataset_path),
                    stack.enter_context(file_dataset_reader(fname)))
                   for fname in fnames]
        # CAREFUL: LowlevelDataReader contains critical C++ structures that are not being
        # tracked by the garbage collector. As soon as this object gets destroyed, those
        # structures will also be destroyed, even if other objects still reference it.
        # This variable assignment makes sure that the reader will exist until the end of
        # the `with` block.
        # If the Python runtime ever becomes clever and eliminates this variable, a different
        # method of keeping the object around should be found.
        dr = LowlevelDataReader(dataset_path, dataset)
        yield dr

ProofStateId = int
TacticId = int
ctypedef uint32_t GraphId
ctypedef uint32_t NodeId
NodeHash = int

cdef class Node:
    """A node in the calculus of inductive construction graph.

    A node has a `label`, a unique `identity` and `children`. Some nodes are also
    a `Definition`.
    """

    cdef GraphIndex *graph_index
    cdef C_Graph_Node_Reader node

    cdef readonly GraphId graph
    """The id of the graph in which this node occurs. For lowlevel use only."""

    cdef readonly NodeId nodeid
    """The local id of the node in the dataset. For lowlevel use only."""

    @staticmethod
    cdef init(GraphId graph, int nodeid, GraphIndex *graph_index):
        cdef Node wrapper = Node.__new__(Node)
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        wrapper.nodeid = nodeid
        wrapper.node = graph_index.nodes[graph][nodeid]
        return wrapper

    def __repr__(self):
        return f"node-{self.graph}-{self.nodeid}"

    def __eq__(self, other: Node) -> bool:
        """Physical equality of two nodes. Note that two nodes with a different physical equality
        may have the same `identity`. See `identity` for details.
        """
        if isinstance(other, Node):
            return self.graph == other.graph and self.nodeid == other.nodeid
        return False

    def __hash__(self):
        """A hash that is corresponds to physical equality, not to be confused by `identity`."""
        return (<uint64_t> self.graph) << 32 | self.nodeid

    @property
    def label(self) -> Any:
        """The label of the node, indicating it's function in the CIC graph."""
        return Graph_Node_Label_Reader.init(self.node.getLabel())

    @property
    def identity(self) -> NodeHash:
        """The identity of a node uniquely determines it. That is, one can consider any to nodes with the same
        identity to be equal. The notion of equal we use is as follows:
        1. Graph perspective: Two nodes have the same identity if and only if they are bisimilar.
           In this notion, bisimilarity does take into consideration the label of the nodes, modulo some
           equivalence class that is not fully specified here. One aspect of the equivalence class is that
           for definition nodes their associated global context (accessed through `Definition.previous`) is
           not taken into account.
        2. Lambda calculus perspective: Two nodes have the same identity if their corresponding lambda terms
           are alpha-equivalent. Note that two definitions with the same body are not considered
           alpha-equivalent.

        The identity of a node is used to perform partial graph-sharing. That is, two nodes with the same
        identity are merged when the graph is generated. There are two reasons why two nodes with the same
        semantic identity might have a different physical identity:
        1. Nodes are only merged when the first node exists in the same graph as the second node, or exists
           in a dependent graph. Hence, nodes originating from developments that do not depend on each other
           are never merged. Full graph-sharing would require global analysis on a dataset, which any consumer
           can optionally do as a post-processing step.
        2. Two definition nodes with the same name and body have the same identity. But if they occur in
           different global contexts, these nodes are physically different to ensure the integrity of their
           global contexts.

        Beware that the identity is currently a 64 bit field. For datasets that have graphs of size in the
        order of billions of nodes there is a non-trivial chance of a collision. (We consider this acceptable
        for machine learning purposes.)
        """
        return self.node.getIdentity()

    @property
    def children(self) -> Sequence[tuple[int, Node]]:
        """The children of a node, together with the labels of the edges towards the children.
        Note that for efficiency purposes, the label is represented as an integer. The corresponding
        enum of this integer is `graph_api_capnp.EdgeClassification`."""
        node = self.node
        graph_index = self.graph_index
        graph = self.graph
        return EdgeTarget_List.init(
            graph_index.edges[graph], graph_index, graph,
            node.getChildrenIndex(), node.getChildrenCount())

    @property
    def definition(self) -> Definition | None:
        """Some nodes in the CIC graph represent definitions. Such nodes contain extra information about the
        definition.
        """
        label = self.node.getLabel()
        if label.isDefinition():
            return Definition.init(self, label.getDefinition(), self.graph, self.graph_index)
        else:
            return None

    @property
    def path(self) -> Path:
        """The physical location on disk in which this node can be found."""
        temp = self.graph_index.datasets[self.graph].getDependencies()[0]
        return Path((<char*>temp.begin())[:temp.size()])

cdef class EdgeTarget_List:
    cdef GraphIndex *graph_index
    cdef C_Graph_EdgeTarget_Reader_List edges
    cdef GraphId graph
    cdef uint32_t start
    cdef uint16_t count

    @staticmethod
    cdef init(C_Graph_EdgeTarget_Reader_List edges, GraphIndex *graph_index,
              GraphId graph, uint32_t start, uint16_t count):
        cdef EdgeTarget_List wrapper = EdgeTarget_List.__new__(EdgeTarget_List)
        wrapper.edges = edges
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        wrapper.start = start
        wrapper.count = count
        return wrapper

    def __getitem__(self, uint index):
        if index >= self.count:
            raise IndexError('Out of bounds')
        cdef C_Graph_EdgeTarget_Reader edge = self.edges[self.start+index]
        cdef C_Graph_EdgeTarget_Target_Reader target = edge.getTarget()
        cdef GraphIndex *graph_index = self.graph_index
        return (edge.getLabel(),
                Node.init(graph_index.local_to_global[self.graph][target.getDepIndex()],
                          target.getNodeIndex(), graph_index))

    def __len__(self):
        return self.count

cdef class Node_List:
    cdef GraphIndex *graph_index
    cdef C_Node_Reader_List reader
    cdef GraphId graph

    @staticmethod
    cdef init(C_Node_Reader_List reader, GraphIndex *graph_index, GraphId graph):
        cdef Node_List wrapper = Node_List.__new__(Node_List)
        wrapper.reader = reader
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')
        node = reader[index]
        graph_index = self.graph_index
        return Node.init(graph_index.local_to_global[self.graph][node.getDepIndex()],
                         node.getNodeIndex(), graph_index)

    def __len__(self):
        return self.reader.size()

cdef class ProofState:
    """A proof state represents a particular point in the tactical proof of a constant."""

    cdef C_ProofState_Reader reader
    cdef GraphId graph
    cdef GraphIndex *graph_index

    @staticmethod
    cdef init(C_ProofState_Reader reader, GraphId graph, GraphIndex *graph_index):
        cdef ProofState wrapper = ProofState.__new__(ProofState)
        wrapper.reader = reader
        wrapper.graph = graph
        wrapper.graph_index = graph_index
        return wrapper

    @property
    def lowlevel(self):
        return ProofState_Reader.init(self.reader)
    def __repr__(self):
        return repr(self.lowlevel)

    @property
    def root(self) -> Node:
        """The entry-point of the proof state, all nodes that are 'part of' the proof state are
        reachable from here."""
        root = self.reader.getRoot()
        graph_index = self.graph_index
        return Node.init(graph_index.local_to_global[self.graph][root.getDepIndex()],
                         root.getNodeIndex(), graph_index)

    @property
    def context(self) -> Sequence[Node]:
        """The local context of the proof state, given as a tuple of their original name as they appeared in the
        proof and the corresponding node.

        The nodes always have either label `contextAssum` or `contextDef`.
        Note that these nodes are also reachable from the root of the proof state.

        The names of the context elements should be used for debugging and viewing purposes only, because
        hypothesis-generating tactics have been modified to use auto-generated names. Hence, tactics should
        not be concerned about the names of the context.
        """
        return Node_List.init(self.reader.getContext(), self.graph_index, self.graph)

    @property
    def context_names(self) -> Sequence[str]:
        return String_List.init(self.reader.getContextNames())

    @property
    def context_text(self) -> Sequence[str]:
        return String_List.init(self.reader.getContextText())

    @property
    def conclusion_text(self) -> str:
        temp = self.reader.getConclusionText()
        return (<char*>temp.begin())[:temp.size()]

    @property
    def text(self) -> str:
        """A textual representation of the proof state."""
        temp = self.reader.getText()
        return (<char*>temp.begin())[:temp.size()]

    def __str__(self) -> str:
        return self.text

    @property
    def id(self) -> ProofStateId:
        """
        A unique identifier of the proof state. Any two proof states in a tactical proof that have an equal id
        can morally be regarded to be 'the same' proof state.
        IMPORTANT: Two proof states with the same id may still have different contents. This is because proof states
                   can contain existential variables (represented by the `evar` node) that can be filled as a
                   side-effect by a tactic running on another proof state.
        """
        return self.reader.getId()

cdef class ProofState_List:
    cdef GraphIndex *graph_index
    cdef C_ProofState_Reader_List reader
    cdef GraphId graph

    @staticmethod
    cdef init(C_ProofState_Reader_List reader, GraphIndex *graph_index, GraphId graph):
        cdef ProofState_List wrapper = ProofState_List.__new__(ProofState_List)
        wrapper.reader = reader
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')
        return ProofState.init(reader[index], self.graph, self.graph_index)

    def __len__(self):
        return self.reader.size()

cdef class Argument_List:
    cdef GraphIndex *graph_index
    cdef C_Argument_Reader_List reader
    cdef GraphId graph

    @staticmethod
    cdef init(C_Argument_Reader_List reader, GraphIndex *graph_index, GraphId graph):
        cdef Argument_List wrapper = Argument_List.__new__(Argument_List)
        wrapper.reader = reader
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')
        arg = reader[index]
        if arg.isUnresolvable():
            return None
        elif arg.isTerm():
            term = arg.getTerm()
            graph_index = self.graph_index
            return Node.init(graph_index.local_to_global[self.graph][term.getDepIndex()],
                             term.getNodeIndex(), graph_index)
        else: assert False

    def __len__(self):
        return self.reader.size()

Unresolvable: TypeAlias = None
Unknown: TypeAlias = None
cdef class Outcome:
    """An outcome is the result of running a tactic on a proof state. A tactic may run on multiple proof states."""

    cdef C_Outcome_Reader reader
    cdef GraphId graph
    cdef GraphIndex *graph_index
    cdef readonly object tactic # : Tactic_Reader | Unknown
    """The tactic that generated the outcome. For it's arguments, see `tactic_arguments`

    Sometimes a tactic cannot or should not be recorded. In those cases, it is marked as 'unknown'.
    This currently happens with tactics that are run as a result of the `Proof with tac` construct and it
    happens for tactics that are known to be unsafe like `change_no_check`, `fix`, `cofix` and more.
    """

    @staticmethod
    cdef init(C_Outcome_Reader reader, tactic: Tactic_Reader | Unknown, GraphId graph, GraphIndex *graph_index):
        cdef Outcome wrapper = Outcome.__new__(Outcome)
        wrapper.reader = reader
        wrapper.tactic = tactic
        wrapper.graph = graph
        wrapper.graph_index = graph_index
        return wrapper

    @property
    def lowlevel(self):
        return Outcome_Reader.init(self.reader)
    def __repr__(self):
        return repr(self.lowlevel)

    @property
    def before(self) -> ProofState:
        """The proof state before the tactic execution."""
        return ProofState.init(self.reader.getBefore(), self.graph, self.graph_index)

    @property
    def after(self) -> Sequence[ProofState]:
        """The new proof states that were generated by the tactic."""
        return ProofState_List.init(self.reader.getAfter(), self.graph_index, self.graph)

    @property
    def term(self) -> Node:
        """The proof term that witnesses the transition from the before state to the after states. It contains a
        hole (an `evar` node) for each of the after states. It may also refer to elements of the local context of
        the before state.
        """
        term = self.reader.getTerm()
        return Node.init(self.graph_index.local_to_global[self.graph][term.getDepIndex()],
                         term.getNodeIndex(), self.graph_index)

    @property
    def term_text(self) -> str:
        """A textual representation of the proof term."""
        temp = self.reader.getTermText()
        return (<char*>temp.begin())[:temp.size()]

    @property
    def tactic_arguments(self) -> Sequence[Node | Unresolvable]:
        """The arguments of the tactic that produced this outcome.

        The node is the root of a graph representing an argument that is a term in the calculus of constructions.
        Sometimes, an argument is not resolvable to a term, in which case it is marked as `Unresolvable`.
        """
        graph = self.graph
        graph_index = self.graph_index
        args = self.reader.getTacticArguments()
        return Argument_List.init(args, graph_index, graph)

cdef class Outcome_List:
    cdef GraphIndex *graph_index
    cdef C_Outcome_Reader_List reader
    cdef GraphId graph
    cdef object tactic

    @staticmethod
    cdef init(C_Outcome_Reader_List reader, object tactic, GraphIndex *graph_index, GraphId graph):
        cdef Outcome_List wrapper = Outcome_List.__new__(Outcome_List)
        wrapper.reader = reader
        wrapper.tactic = tactic
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')
        return Outcome.init(reader[index], self.tactic, self.graph, self.graph_index)

    def __len__(self):
        return self.reader.size()

cdef class ProofStep:
    """A proof step is the execution of a single tactic on one or more proof states, producing a list of outcomes.
    """

    cdef C_ProofStep_Reader reader
    cdef GraphId graph
    cdef GraphIndex *graph_index

    @staticmethod
    cdef init(C_ProofStep_Reader reader, GraphId graph, GraphIndex *graph_index):
        cdef ProofStep wrapper = ProofStep.__new__(ProofStep)
        wrapper.reader = reader
        wrapper.graph = graph
        wrapper.graph_index = graph_index
        return wrapper

    @property
    def lowlevel(self):
        return ProofStep_Reader.init(self.reader)
    def __repr__(self):
        return repr(self.lowlevel)

    @property
    def tactic(self) -> Tactic_Reader | Unknown:
        """The tactic that generated the proof step. Note that the arguments of the tactic can be found in the
        individual outcomes, because they may be different for each outcome.

        Sometimes a tactic cannot or should not be recorded. In those cases, it is marked as 'unknown'.
        This currently happens with tactics that are run as a result of the `Proof with tac` construct and it
        happens for tactics that are known to be unsafe like `change_no_check`, `fix`, `cofix` and more.
        """
        tactic = self.reader.getTactic()
        if tactic.isUnknown():
            return None
        elif tactic.isKnown():
            return Tactic_Reader.init(tactic.getKnown())
        else: assert False

    @property
    def outcomes(self) -> Sequence[Outcome]:
        """A list of transformations of proof states to other proof states, as executed by the tactic of the proof
        step
        """
        return Outcome_List.init(self.reader.getOutcomes(), self.tactic, self.graph_index, self.graph)

cdef class ProofStep_List:
    cdef GraphIndex *graph_index
    cdef C_ProofStep_Reader_List reader
    cdef GraphId graph

    @staticmethod
    cdef init(C_ProofStep_Reader_List reader, GraphIndex *graph_index, GraphId graph):
        cdef ProofStep_List wrapper = ProofStep_List.__new__(ProofStep_List)
        wrapper.reader = reader
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')
        return ProofStep.init(reader[index], self.graph, self.graph_index)

    def __len__(self):
        return self.reader.size()

cdef class Representative_List:
    cdef GraphIndex *graph_index
    cdef C_Uint16_List reader
    cdef GraphId graph

    @staticmethod
    cdef init(C_Uint16_List reader, GraphIndex *graph_index, GraphId graph):
        cdef Representative_List wrapper = Representative_List.__new__(Representative_List)
        wrapper.reader = reader
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')

        graph_index = self.graph_index
        depgraph = graph_index.local_to_global[self.graph][reader[index]]
        return cast(Definition, Node.init(depgraph, graph_index.datasets[depgraph].getRepresentative(),
                                          graph_index).definition)

    def __len__(self):
        return self.reader.size()

@dataclass
class Original: pass
@dataclass
class Discharged:
    original: Definition
@dataclass
class Substituted:
    original: Definition

@dataclass
class Inductive:
    representative: Definition
@dataclass
class Constructor:
    representative: Definition
@dataclass
class Projection:
    representative: Definition
@dataclass
class ManualConstant: pass
@dataclass
class TacticalConstant:
    proof: Sequence[ProofStep]
@dataclass
class ManualSectionConstant: pass
@dataclass
class TacticalSectionConstant:
    proof: Sequence[ProofStep]

cdef class Definition:
    """A definition of the CIC, which is either an constant, inductive, constructor, projection or section
    variable. Constants and section variables can have tactical proofs associated to them.
    """

    cdef C_Definition_Reader reader
    cdef GraphId graph
    cdef GraphIndex *graph_index
    cdef readonly Node node
    """The node that is associated with this definition. It holds that `definition.node.definition == definition`.
    """

    @staticmethod
    cdef init(Node node, C_Definition_Reader reader, GraphId graph, GraphIndex *graph_index):
        cdef Definition wrapper = Definition.__new__(Definition)
        wrapper.node = node
        wrapper.reader = reader
        wrapper.graph = graph
        wrapper.graph_index = graph_index
        return wrapper

    @property
    def lowlevel(self):
        return Definition_Reader.init(self.reader)
    def __repr__(self):
        return repr(self.lowlevel)

    def __eq__(self, other: Definition) -> bool:
        """Physical equality of two nodes. Note that two nodes with a different physical equality
        may have the same `identity`. See `identity` for details.
        """
        if isinstance(other, Definition):
            return self.node == other.node
        return False

    def __hash__(self):
        """A hash that is corresponds to physical equality, not to be confused by `identity`."""
        return hash(self.node)

    @property
    def name(self) -> str:
        """The fully-qualified name of the definition. The name should be unique in a particular global context,
        but is not unique among different branches of the global in a dataset.
        """
        temp = self.reader.getName()
        return (<char*>temp.begin())[:temp.size()]

    @property
    def previous(self) -> Definition | None:
        """The previous definition within the global context of the current file.

        Note that this is a lowlevel property. Prefer to use `global_context` or `clustered_global_context`.

        The contract on this field is that any definition nodes reachable from the forward closure of the definition
        must also be reachable through the chain of previous fields. An exception to this rule are mutually
        recursive definitions. Those nodes are placed into the global context in an arbitrary ordering.
        """
        nodeid = self.reader.getPrevious()
        if self.graph_index.nodes[self.graph].size() == nodeid:
            return None
        else:
            node = Node.init(self.graph, nodeid, self.graph_index)
            return node.definition

    @property
    def external_previous(self) -> Sequence[Definition]:
        """A list of definitions that are the representatives of files other than the current file that are
        part of the global context. This list becomes populated when a `Require` statement occurred right before
        the definition.

        Note that this is a lowlevel property. Prefer to use `global_context` or `clustered_global_context`.
        """
        return Representative_List.init(self.reader.getExternalPrevious(), self.graph_index, self.graph)

    def _global_context(self, across_files : bool, inclusive, seen : set[Node]):
        # force inclusivity for recursive definitions
        d = self.cluster_representative
        if isinstance(d.kind, (Inductive, Constructor, Projection)):
            inclusive = True

        while d is not None:
            if inclusive: yield d # skip first result if not inclusive
            else: inclusive = True
            if across_files:
                for eprev in d.external_previous:
                    if eprev.node in seen: continue
                    yield from eprev._global_context(
                        inclusive = True,
                        across_files = True,
                        seen = seen,
                    )
                    seen.add(eprev.node)
            d = d.previous

    def global_context(self, across_files : bool = True, inclusive = False) -> Iterable[Definition]:
        """All of the definitions in the global context when this definition was created.

        Note that this does not include this definition itself, except when the definition is a inductive,
        constructor or projection. Because those are mutually recursive objects, they reference themselves
        and are therefore part of their own global context.

        Arguments:
        * across_files -- if False, outputs only the definitions from the local file, default True
        * inclusive -- if True, outputs also itself, default False
        Note: if it is a self-recursive definition,
        the inclusive argument is ignored, and considered as True
        """
        return self._global_context(
            across_files = across_files,
            inclusive = inclusive,
            seen = set(),
        )

    def clustered_global_context(self, across_files : bool = True, inclusive : bool = False) -> Iterable[list[Definition]]:
        """All of the definitions in the global context when this definition was created, clustered into
        mutually recursive cliques.

        Arguments:
        * across_files -- if False, outputs only the definitions from the local file, default True
        * inclusive -- if True, outputs also the cluster of itself, default False
        Note: if it is a self-recursive definition,
        the inclusive argument is ignored, and considered as True
        """
        return self._group_by_clusters(self.global_context(
            across_files = across_files,
            inclusive = inclusive,
        ))

    @property
    def status(self) -> Original | Discharged | Substituted:
        """A definition is either
        (1) an object as originally inputted by the user.
        (2) a definition that was originally defined in a section and has now had the section
            variables discharged into it.
        (3) a definition that was obtained by performing some sort of module functor substitution.
        When a definition is not original, we cross-reference to the definition that it was derived from.
        """
        status = self.reader.getStatus()
        if status.isOriginal():
            return Original()
        elif status.isDischarged():
            return Discharged(
                cast(Definition, Node.init(self.graph, status.getDischarged(),
                                           self.graph_index).definition))
        elif status.isSubstituted():
            substituted = status.getSubstituted()
            return Substituted(
                cast(Definition,
                     Node.init(self.graph_index.local_to_global[self.graph][substituted.getDepIndex()],
                               substituted.getNodeIndex(), self.graph_index).definition))
        else: assert False

    @property
    def kind(self) -> Union[Inductive, Constructor, Projection,
                            ManualConstant, TacticalConstant,
                            ManualSectionConstant, TacticalSectionConstant]:
        """The kind of the definition.

        It can be an constant, section constant, inductive, constructor or projection. In case of a constant
        of a constant or section constant, an optional tactical proof can be associated. In case of an inductive,
        constructor or projection, another definition that acts as the representative of the mutually recursive
        cluster of definitions is associated.

        The associated information is low-level. Prefer to use the properties `proof` and `cluster` to access them.
        """
        kind = self.reader
        graph = self.graph
        graph_index = self.graph_index
        if kind.isInductive():
            return Inductive(
                cast(Definition, Node.init(self.graph, kind.getInductive(),
                                           self.graph_index).definition))
        elif kind.isConstructor():
            return Inductive(
                cast(Definition, Node.init(self.graph, kind.getConstructor(),
                                           self.graph_index).definition))
        elif kind.isProjection():
            return Projection(
                cast(Definition, Node.init(self.graph, kind.getProjection(),
                                           self.graph_index).definition))
        elif kind.isManualConstant():
            return ManualConstant()
        elif kind.isTacticalConstant():
            return TacticalConstant(ProofStep_List.init(kind.getTacticalConstant(), graph_index, graph))
        elif kind.isManualSectionConstant():
            return ManualConstant()
        elif kind.isTacticalSectionConstant():
            return TacticalSectionConstant(ProofStep_List.init(kind.getTacticalSectionConstant(), graph_index, graph))
        else: assert False

    @property
    def proof(self) -> Sequence[ProofStep] | None:
        """An optional tactical proof that was used to create this definition."""
        kind = self.kind
        if isinstance(kind, (TacticalConstant, TacticalSectionConstant)):
            return kind.proof
        else: return None

    @property
    def cluster_representative(self) -> Definition:
        """A unique representative of the mutually recursive cluster of definitions this definition is part of.
        If the definition is not mutually recursive, the representative is itself.

        This is a low-level property. Prefer to use the `cluster` property.
        """
        kind = self.kind
        if isinstance(kind, (Inductive, Constructor, Projection)):
            return kind.representative
        else: return self

    @property
    def cluster(self) -> Iterable[Definition]:
        """The cluster of mutually recursive definitions that this definition is part of.
        If the definition is not mutually recursive, the cluster is a singleton."""
        representative = self.cluster_representative
        repr_node = representative.node
        current = representative
        while True:
            yield current
            current = current.previous
            if not current:
                break
            if current.cluster_representative.node != repr_node:
                break

    @property
    def type_text(self) -> str:
        """A textual representation of the type of this definition."""
        temp = self.reader.getTypeText()
        return (<char*>temp.begin())[:temp.size()]

    @property
    def term_text(self) -> str | None:
        """A textual representation of the body of this definition.
        For inductives, constructors, projections, section variables and axioms this is `None`.
        """
        temp = self.reader.getTermText()
        cdef str text = (<char*>temp.begin())[:temp.size()]
        if text == "":
            return None
        else:
            return text

    @property
    def is_file_representative(self) -> bool:
        """Returns true if this definition is the representative of the super-global context of it's file.
        Se also `Dataset.representative`."""
        return self.graph_index.datasets[self.graph].getRepresentative() == self.node.nodeid

    @classmethod
    def _group_by_clusters(cls, definitions : Iterable[Definition]) -> Iterable[list[Definition]]:
        definitions_it = iter(definitions)
        while True:
            try:
                node = next(definitions_it)
            except StopIteration:
                return
            rep = node.cluster_representative
            n = rep
            cluster = [n]
            while n.previous and n.previous.cluster_representative == rep:
                n = n.previous
                cluster.append(n)
                next(definitions_it)
            yield cluster

cdef class Dataset_Definition_List:
    cdef GraphIndex *graph_index
    cdef C_Uint32_List reader
    cdef GraphId graph

    @staticmethod
    cdef init(C_Uint32_List reader, GraphIndex *graph_index, GraphId graph):
        cdef Dataset_Definition_List wrapper = Dataset_Definition_List.__new__(Dataset_Definition_List)
        wrapper.reader = reader
        wrapper.graph_index = graph_index
        wrapper.graph = graph
        return wrapper

    def __getitem__(self, uint index):
        reader = self.reader
        if index >= reader.size():
            raise IndexError('Out of bounds')
        return cast(Definition, Node.init(self.graph, reader[index], self.graph_index).definition)

    def __len__(self):
        return self.reader.size()

cdef class Dataset:
    """The data corresponding to a single Coq source file. The data contains a representation of all definitions
    that have existed at any point throughout the compilation of the source file.
    """

    cdef GraphIndex *graph_index
    cdef GraphId graph
    cdef C_Dataset_Reader reader
    cdef readonly object filename # : Path
    """The physical file in which the data contained in this class can be found."""

    @staticmethod
    cdef init(filename: Path, GraphId graph, GraphIndex *graph_index):
        cdef Dataset wrapper = Dataset.__new__(Dataset)
        wrapper.filename = filename
        wrapper.graph = graph
        wrapper.reader = graph_index.datasets[graph]
        wrapper.graph_index = graph_index
        return wrapper

    @property
    def lowlevel(self):
        return Dataset_Reader.init(self.reader)
    def __repr__(self):
        return repr(self.lowlevel)

    @property
    def dependencies(self) -> Sequence[Path]:
        """A list of physical paths of data files that are direct dependencies of this file."""
        dependencies = self.reader.getDependencies()
        # The first dependency is the file itself, which we do not want to expose here.
        count = dependencies.size() - 1
        def get(index):
            temp = dependencies[index+1]
            return Path((<char*>temp.begin())[:temp.size()])
        return TupleLike(count, get)

    @property
    def representative(self) -> Definition | None:
        """The entry point of the global context of definitions that are available when this file is 'Required' by
        another file. The full global context can be obtained by following the `previous` node of definitions.
        If the compilation unit does not contain any 'super'-global definitions this may be `None`.

        This is a low-level property.
        Prefer to use `definitions(spine_only = True)` and `clustered_definitions(spine_only=True)`.
        """
        representative = self.reader.getRepresentative()
        if self.reader.getGraph().getNodes().size() == representative:
            return None
        else:
            return Node.init(self.graph, representative, self.graph_index).definition

    def definitions(self, across_files = False, spine_only = False) -> Iterable[Definition]:
        """All of the definitions present in the file.
        Note that some of these nodes may not be part of the 'super-global' context. Those are definitions inside
        of sections or module functors.

        Arguments:
        * across_files -- if True, outputs also definitions from dependent files, default False
        * spine_only -- if True, outputs only the definitions on the main spine, default False
        Note: across_files = True is incompatible with spine_only = False
        """
        if across_files and not spine_only:
            raise Exception("Options across_files = True and spine_only = False are incompatible")
        if spine_only:
            if self.representative is None: return ()
            return self.representative.global_context(
                inclusive = True,
                across_files = across_files,
            )
        else:
            return Dataset_Definition_List.init(self.reader.getDefinitions(), self.graph_index, self.graph)

    def clustered_definitions(self, across_files = False, spine_only = False) -> Iterable[list[Definition]]:
        """All of the definitions present in the file, clustered by mutually recursive definitions.
        Note that some of these nodes may not be part of the 'super-global' context. Those are definitions inside
        of sections or module functors.

        Arguments:
        * across_files -- if True, outputs also definitions from dependent files, default False
        * spine_only -- if True, outputs only the definitions on the main spine, default False
        Note: across_files = True is incompatible with spine_only = False
        """
        return Definition._group_by_clusters(
            self.definitions(
                across_files = across_files,
                spine_only = spine_only
            )
        )

    @property
    def module_name(self) -> str:
        temp = self.reader.getModuleName()
        return (<char*>temp.begin())[:temp.size()]

    def node_by_id(self, nodeid: NodeId) -> Node:
        """Lookup a node inside of this file by it's local node-id. This is a low-level function."""
        return Node.init(self.graph, nodeid, self.graph_index)

def lowlevel_to_highlevel(LowlevelDataReader lreader) -> dict[Path, Dataset]:
    """Convert a dataset initialized as a `LowLevelDataReader` into a high level interface."""
    return {f: Dataset.init(f, g, &lreader.graph_index)
            for f, g in lreader.graphid_by_filename.items()}

@contextmanager
def data_reader(dataset_path: Path) -> Generator[dict[Path, Dataset], None, None]:
    """Load a directory of dataset files into memory, and expose the data they contain.
    The result is a dictionary that maps physical paths to `Dataset` instances that allow access to the data.
    """
    with lowlevel_data_reader(dataset_path) as lreader:
        yield lowlevel_to_highlevel(lreader)

class GlobalContextSets:
    """Lazily retrieve a the global context of a definition as a set, with memoization.

    Because this class can allocate (large) amounts of memory, use it as a context-manager. To create a new
    instance, and retrieve the context of a definition, use:
    ```
    with GlobalContextSets.new_context() as gcs:
        gcs.global_context_set(my_definition)
    ```
    This context will be cached, as well as intermediate results (global contexts of other definitions) as
    long as the context-set is in scope. Caching can be nested by using `sub_context`:
    ```
    with GlobalContextSets.new_context() as gcs:
        gcs.global_context_set(my_definition)
        with gcs.sub_context(lambda d:is_file_representative) as gcs_sub:
            # gcs_sub remembers anything in the case of gcs
            # caching is propagated to the parent cache only if the provided lambda evaluates to true
            gcs_sub.global_context_set(my_definition2)
    ```
    """
    def __init__(self, cache, parent: GlobalContextSets | None,
                 should_propagate: Callable[[Definition], bool]):
        """Do not call directly. Use `GlobalContextSets.new_context` and `GlobalContextSets.sub_context`."""
        self.cache: Map = cache
        self.parent = parent
        self.should_propagate = should_propagate

    def _propagate(self, d: Definition, context: PSet):
        self.cache = self.cache.set(d, context)
        if self.should_propagate(d) and (parent := self.parent):
            parent._propagate(d, context)

    def _global_context_set(self, d: Definition) -> PSet:
        try:
            return self.cache[d]
        except KeyError:
            external_previous = list(d.external_previous)
            if prev := d.previous:
                new_set = self._global_context_set(prev).set(prev, ())
            elif len(external_previous) > 0:
                ep = external_previous[0]
                new_set = self._global_context_set(ep).set(ep, ())
                external_previous = external_previous[1:]
            else:
                new_set = Map()
            for ep in external_previous:
                new_set = new_set.update(self._global_context_set(ep)).set(ep, ())
            self._propagate(d, new_set)
            return new_set

    def global_context_set(self, d: Definition) -> PSet:
        """Retrieve the global context of a definition, caching the result, and intermediate results."""
        start = d.cluster_representative
        context = self._global_context_set(start)
        if isinstance(start.kind, (Inductive, Constructor, Projection)):
            context = context.set(start, ())
        return context

    @contextmanager
    def sub_context(self, propagate: Callable[[Definition], bool]) -> Generator[GlobalContextSets, None, None]:
        """Create a sub-context with a separate cache from it's parent, but sharing any info that was
        already present in the parent's cache.

        For any intermediate results for a definition `d` for which `propagate(d)` evaluates to true,
        the result is propagated to the parent's cache."""
        yield GlobalContextSets(self.cache, self, propagate)

    @contextmanager
    @staticmethod
    def new_context() -> Generator[GlobalContextSets, None, None]:
        """Crate a new caching context where global-context-set's can be retrieved and cached."""
        yield GlobalContextSets(Map(), None, lambda _: False)

cdef struct GlobalNode:
    GraphId graphid
    NodeId nodeid

def node_dependencies(Node n, deps: set[Definition] | None = None) -> set[Definition]:
    """Given a `Node` `n`, calculate the set of `Definition`'s that are directly reachable
    from `n`. Transitively reachable definitions are not included.
    """
    cdef GraphIndex *graph_index = n.graph_index
    cdef unordered_set[int64_t] seen
    cdef stack[GlobalNode] stack
    cdef GlobalNode current_node
    cdef uint32_t children_index
    cdef uint16_t children_limit

    if deps is None:
        deps = set()
    stack.push(GlobalNode(n.graph, n.nodeid))
    while not stack.empty():
        current_node = stack.top()
        stack.pop()
        node_hash = (<uint64_t> current_node.graphid) << 32 | current_node.nodeid
        if seen.find(node_hash) != seen.end():
            continue
        seen.insert(node_hash)
        node_contents = graph_index.nodes[current_node.graphid][current_node.nodeid]
        node_label = node_contents.getLabel()
        if node_label.isDefinition():
            deps.add(Node.init(current_node.graphid, current_node.nodeid, graph_index).definition)
            continue
        children_index = node_contents.getChildrenIndex()
        children_limit = children_index + node_contents.getChildrenCount()
        edges = graph_index.edges[current_node.graphid]
        while children_index < children_limit:
            edge_target = edges[children_index].getTarget()
            child_graph = graph_index.local_to_global[current_node.graphid][edge_target.getDepIndex()]
            child = GlobalNode(child_graph, edge_target.getNodeIndex())
            stack.push(child)
            children_index += 1
    return deps

def definition_dependencies(d: Definition):
    """Given a `Definition` `d`, calculate the set of `Definition`'s that are directly reachable for `d`.
    Transitively reachable definitions are not included nor is `d` itself.
    """
    deps = set()
    for _, c in d.node.children:
        node_dependencies(c, deps)
    return deps