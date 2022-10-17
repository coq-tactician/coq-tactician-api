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
"""

from __future__ import annotations
import mmap
from contextlib import contextmanager, ExitStack
from dataclasses import dataclass
from typing import Any, TypeAlias, Union, cast
from collections.abc import Iterable, Sequence, Generator
from pathlib import Path
import capnp
from pytact.common import graph_api_capnp

capnp.remove_import_hook()  # pyright: ignore
graph_api_capnp = capnp.load(graph_api_capnp())  # pyright: ignore

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
                yield g


class LowlevelDataReader(Sequence[Any]):
    """A thin wrapper around the raw Cap'n Proto structures contained in a dataset directory.

    Every file in the directory is assigned an integer called a graph-id. This class is a
    `Sequence` that allows the retrieval of the structures in a file by its graph-id.
    Additionally, the function `local_to_global` translates a dependency-index relative to
    a graph-id to a new graph-id. This allows one to find out in which file a node in a
    graph is located.

    This is a lowlevel interface. For details on using the exposed structures the documentation
    in the Cap'n Proto API file.
    """

    graphid_by_filename: dict[Path, int]
    """Map from filenames in the data directory to their graph-id's."""

    graph_files: list[Path]
    """Map's a graph-id to a filename. Inverse of `graphid_by_filename`."""

    def __init__(self, dataset_path : Path, dataset: list[tuple[Path, Any]]):
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

        self.__graphs = [g for _, g in dataset]
        self.graphs_by_filename = {f: i for i, (f, _) in enumerate(dataset)}
        self.graph_files = [f for f, _ in dataset]
        self.__local_to_global = [[self.graphs_by_filename[Path(f)] for f in g.dependencies] for _, g in dataset]

    def local_to_global(self, graph: int, dep_index: int) -> int:
        """Convert dependency-index relative to a graph-id to a new graph-id.

        This is used to find the graph-id where a particular node can be found. If `graph` is the
        graph-id that contains a reference to a node and `dep_index` is the relative location
        where that node can be found then `local_to_global(graph, dep_index)` finds the graph-id
        of the file where the node is physically located.
        """
        return self.__local_to_global[graph][dep_index]

    def __getitem__(self, graph):
        """Retrieve the raw Cap'n Proto structures associated with a graph-id."""
        return self.__graphs[graph]

    def __len__(self) -> int:
        return len(self.__graphs)

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
        yield LowlevelDataReader(dataset_path, dataset)


ProofStateId = int
TacticId = int
GraphId = int
NodeId = int
NodeHash = int


class Node:
    """A node in the calculus of inductive construction graph.

    A node has a `label`, a unique `identity` and `children`. Some nodes are also
    a `Definition`.
    """

    nodeid: NodeId
    """The local id of the node in the dataset. For lowlevel use only."""

    def __init__(self, graph: GraphId, nodeid: NodeId, lreader: LowlevelDataReader):
        self.__lreader = lreader
        self.__graph = graph
        self.nodeid = nodeid
        self.__node = lreader[graph].graph.nodes[nodeid]

    def __repr__(self):
        return f"node-{self.__graph}-{self.nodeid}"

    def __eq__(self, other: Node) -> bool:
        """Physical equality of two nodes. Note that two nodes with a different physical equality
        may have the same `identity`. See `identity` for details.
        """
        return self.__graph == other.__graph and self.nodeid == other.nodeid

    def __hash__(self):
        """A hash that is corresponds to physical equality, not to be confused by `identity`."""
        return hash((self.__graph, self.nodeid))

    @property
    def label(self) -> Any:
        """The label of the node, indicating it's function in the CIC graph."""
        return self.__node.label

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
        return self.__node.identity

    @property
    def children(self) -> Sequence[tuple[Any, Node]]:
        """The children of a node, together with the labels of the edges towards the children."""
        node = self.__node
        lreader = self.__lreader
        graph = self.__graph
        edges = lreader[self.__graph].graph.edges
        start = node.childrenIndex
        count = node.childrenCount
        class Seq(Sequence[tuple[Any, Node]]):
            def __getitem__(self, index: int) -> tuple[Any, Node]:
                if index >= count:
                    raise IndexError()
                edge = edges[start+index]
                return (edge.label,
                   Node(lreader.local_to_global(graph, edge.target.depIndex),
                        edge.target.nodeIndex, lreader))
            def __len__(self) -> int:
                return count
        return Seq()

    @property
    def definition(self) -> Definition | None:
        """Some nodes in the CIC graph represent definitions. Such nodes contain extra information about the
        definition.
        """
        if graph_api_capnp.Graph.Node.Label.definition == self.label.which:
            return Definition(self, self.label.definition, self.__graph, self.__lreader)
        else:
            return None

    @property
    def path(self) -> Path:
        """The physical location on disk in which this node can be found."""
        return Path(self.__lreader[self.__graph].dependencies[0])

class Tactic:
    """A concrete tactic with it's parameters determined.

    Somewhat counterintuitively, this class does not actually include these parameters. They can instead be
    found in `Outcome.tactic_arguments`. The reason for this is that one tactic can run on multiple proof
    states at the same time and for all of those proof states, the arguments may be resolved differently.
    """

    def __init__(self, reader):
        self.__reader = reader

    def __repr__(self) -> str:
        return repr(self.__reader)

    @property
    def ident(self) -> TacticId:
        """A hash representing the identity of a tactic without it's arguments. Due to the complexity of the syntax
        trees of Coq's tactics, we do not currently encode the syntax tree. Instead, this hash is a representative
        of the syntax tree of the tactic with all of it's arguments removed.
        """
        return self.__reader.ident

    @property
    def text(self) -> str:
        """The full text of the tactic including the full arguments. This does not currently correspond to
        (ident, arguments) because in this dataset arguments do not include full terms, but only references to
        definitions and local context elements.
        """
        return self.__reader.text

    def __str__(self) -> str:
        return self.__reader.text

    @property
    def base_text(self) -> str:
        """A textual representation of the base tactic without arguments. It tries to roughly correspond to `ident`.
        Note, however, that this is both an under-approximation and an over-approximation. The reason is that tactic
        printing is not 100% isomorphic to Coq's internal AST of tactics. Sometimes, different tactics get mapped to
        the same text. Conversely, the same tactic may be mapped to different texts when identifiers are printed
        using different partially-qualified names.
        """
        return self.__reader.baseText

    @property
    def interm_text(self) -> str:
        """A textual representation that tries to come as close as possible to (ident, arguments).
        It comes with the same caveats as `baseText`.
        """
        return self.__reader.intermText

    @property
    def exact(self) -> bool:
        """Indicates whether or not `ident` + `arguments` is faithfully reversible into the original "strictified"
        tactic. Note that this does not necessarily mean that it represents exactly the tactic that was inputted by
        the user. All tactics are modified to be 'strict' (meaning that tactics that have delayed variables in them
        break). This flag measures the faithfulness of the representation w.r.t. the strict version of the tactic,
        not the original tactic inputted by the user.
        """
        return self.__reader.exact

class ProofState:
    """A proof state represents a particular point in the tactical proof of a constant."""

    def __init__(self, reader, graph: GraphId, lreader: LowlevelDataReader):
        self.__reader = reader
        self.__graph = graph
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    @property
    def root(self) -> Node:
        """The entry-point of the proof state, all nodes that are 'part of' the proof state are
        reachable from here."""
        root = self.__reader.root
        lreader = self.__lreader
        return Node(lreader.local_to_global(self.__graph, root.depIndex),
                    root.nodeIndex, self.__lreader)

    @property
    def context(self) -> Sequence[tuple[str, Node]]:
        """The local context of the proof state, given as a tuple of their original name as they appeared in the
        proof and the corresponding node.

        The nodes always have either label `contextAssum` or `contextDef`.
        Note that these nodes are also reachable from the root of the proof state.

        The names of the context elements should be used for debugging and viewing purposes only, because
        hypothesis-generating tactics have been modified to use auto-generated names. Hence, tactics should
        not be concerned about the names of the context.
        """
        graph = self.__graph
        lreader = self.__lreader
        context = self.__reader.context
        context_names = self.__reader.contextNames
        count = len(context)
        class Seq(Sequence[tuple[str, Node]]):
            def __getitem__(self, index: int) -> tuple[str, Node]:
                if index >= count:
                    raise IndexError()
                n = context[index]
                return (context_names[index],
                        Node(lreader.local_to_global(graph, n.depIndex),
                             n.nodeIndex, lreader))
            def __len__(self) -> int:
                return count
        return Seq()

    @property
    def text(self) -> str:
        """A textual representation of the proof state."""
        return self.__reader.text

    def __str__(self) -> str:
        return self.__reader.text

    @property
    def id(self) -> ProofStateId:
        """
        A unique identifier of the proof state. Any two proof states in a tactical proof that have an equal id
        can morally be regarded to be 'the same' proof state.
        IMPORTANT: Two proof states with the same id may still have different contents. This is because proof states
                   can contain existential variables (represented by the `evar` node) that can be filled as a
                   side-effect by a tactic running on another proof state.
        """
        return self.__reader.id

Unresolvable: TypeAlias = None
Unknown: TypeAlias = None
class Outcome:
    """An outcome is the result of running a tactic on a proof state. A tactic may run on multiple proof states."""

    tactic: Tactic | Unknown
    """The tactic that generated the outcome. For it's arguments, see `tactic_arguments`

    Sometimes a tactic cannot or should not be recorded. In those cases, it is marked as 'unknown'.
    This currently happens with tactics that are run as a result of the `Proof with tac` construct and it
    happens for tactics that are known to be unsafe like `change_no_check`, `fix`, `cofix` and more.
    """

    def __init__(self, reader, tactic: Tactic | Unknown, graph: GraphId, lreader: LowlevelDataReader):
        self.__reader = reader
        self.tactic = tactic
        self.__graph = graph
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def before(self) -> ProofState:
        """The proof state before the tactic execution."""
        return ProofState(self.__reader.before, self.__graph, self.__lreader)

    @property
    def after(self) -> Sequence[ProofState]:
        """The new proof states that were generated by the tactic."""
        graph = self.__graph
        lreader = self.__lreader
        after = self.__reader.after
        count = len(after)
        class Seq(Sequence[ProofState]):
            def __getitem__(self, index: int) -> ProofState:
                if index >= count:
                    raise IndexError()
                return ProofState(after[index], graph, lreader)
            def __len__(self) -> int:
                return count
        return Seq()

    @property
    def term(self) -> Node:
        """The proof term that witnesses the transition from the before state to the after states. It contains a
        hole (an `evar` node) for each of the after states. It may also refer to elements of the local context of
        the before state.
        """
        term = self.__reader.term
        return Node(self.__lreader.local_to_global(self.__graph, term.depIndex),
                    self.__reader.term.nodeIndex, self.__lreader)

    @property
    def term_text(self) -> str:
        """A textual representation of the proof term."""
        return self.__reader.termText

    @property
    def tactic_arguments(self) -> Sequence[Node | Unresolvable]:
        """The arguments of the tactic that produced this outcome.

        The node is the root of a graph representing an argument that is a term in the calculus of constructions.
        Sometimes, an argument is not resolvable to a term, in which case it is marked as `Unresolvable`.
        """
        graph = self.__graph
        lreader = self.__lreader
        args = self.__reader.tactic_arguments
        count = len(args)
        class Seq(Sequence[Node | Unresolvable]):
            def __getitem__(self, index: int) -> Node | Unresolvable:
                if index >= count:
                    raise IndexError()
                arg = args[index]
                match arg.which:
                    case graph_api_capnp.Argument.unresolvable:
                        return None
                    case graph_api_capnp.Argument.term:
                        return Node(lreader.local_to_global(graph, arg.term.depIndex),
                                    arg.term.nodeIndex, lreader)
            def __len__(self) -> int:
                return count
        return Seq()

class ProofStep:
    """A proof step is the execution of a single tactic on one or more proof states, producing a list of outcomes.
    """

    def __init__(self, reader, graph: GraphId, lreader: LowlevelDataReader):
        self.__reader = reader
        self.__graph = graph
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def tactic(self) -> Tactic | Unknown:
        """The tactic that generated the proof step. Note that the arguments of the tactic can be found in the
        individual outcomes, because they may be different for each outcome.

        Sometimes a tactic cannot or should not be recorded. In those cases, it is marked as 'unknown'.
        This currently happens with tactics that are run as a result of the `Proof with tac` construct and it
        happens for tactics that are known to be unsafe like `change_no_check`, `fix`, `cofix` and more.
        """
        tactic = self.__reader.tactic
        match tactic.which:
            case graph_api_capnp.ProofStep.Tactic.unknown:
                return None
            case graph_api_capnp.ProofStep.Tactic.known:
                return Tactic(tactic.known)

    @property
    def outcomes(self) -> Sequence[Outcome]:
        """A list of transformations of proof states to other proof states, as executed by the tactic of the proof
        step
        """
        graph = self.__graph
        lreader = self.__lreader
        tactic = self.tactic
        outcomes = self.__reader.outcomes
        count = len(outcomes)
        class Seq(Sequence[Outcome]):
            def __getitem__(self, index: int) -> Outcome:
                if index >= count:
                    raise IndexError()
                return Outcome(outcomes[index], tactic, graph, lreader)
            def __len__(self) -> int:
                return count
        return Seq()

class Definition:
    """A definition of the CIC, which is either an constant, inductive, constructor, projection or section
    variable. Constants and section variables can have tactical proofs associated to them.
    """

    node: Node
    """The node that is associated with this definition. It holds that `definition.node.definition == definition`.
    """

    def __init__(self, node: Node, reader, graph: GraphId, lreader: LowlevelDataReader):
        self.node = node
        self.__reader = reader
        self.__graph = graph
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def name(self) -> str:
        """The fully-qualified name of the definition. The name should be unique in a particular global context,
        but is not unique among different branches of the global in a dataset.
        """
        return self.__reader.name

    @property
    def previous(self) -> Definition | None:
        """The previous definition within the global context of the current file.

        Note that this is a lowlevel property. Prefer to use `global_context` or `clustered_global_context`.

        The contract on this field is that any definition nodes reachable from the forward closure of the definition
        must also be reachable through the chain of previous fields. An exception to this rule are mutually
        recursive definitions. Those nodes are placed into the global context in an arbitrary ordering.
        """
        nodeid = self.__reader.previous
        if len(self.__lreader[self.__graph].graph.nodes) == nodeid:
            return None
        else:
            node = Node(self.__graph, nodeid, self.__lreader)
            return node.definition

    @property
    def external_previous(self) -> Sequence[Definition]:
        """A list of definitions that are the representatives of files other than the current file that are
        part of the global context. This list becomes populated when a `Require` statement occurred right before
        the definition.

        Note that this is a lowlevel property. Prefer to use `global_context` or `clustered_global_context`.
        """
        lreader = self.__lreader
        graph = self.__graph
        eps = self.__reader.externalPrevious
        count = len(eps)
        class Seq(Sequence[Definition]):
            def __getitem__(self, index: int) -> Definition:
                if index >= count:
                    raise IndexError()
                depgraph = lreader.local_to_global(graph, eps[index])
                return cast(Definition, Node(depgraph, lreader[depgraph].representative, lreader).definition)
            def __len__(self) -> int:
                return count
        return Seq()

    def __global_context(self, seen: set[Node]) -> Iterable[Definition]:
        if prev := self.previous:
            yield prev
            yield from prev.__global_context(seen)
        for eprev in self.external_previous:
            if eprev.node not in seen:
                yield eprev
                yield from eprev.__global_context(seen)
                seen.add(eprev.node)

    def __clustered_global_context(self, seen: set[Node]) -> Iterable[list[Definition]]:
        if prev := self.previous:
            cluster = list(prev.cluster)
            yield cluster
            yield from cluster[-1].__clustered_global_context(seen)
        for eprev in self.external_previous:
            if eprev.node not in seen:
                cluster = list(eprev.cluster)
                yield cluster
                yield from cluster[-1].__clustered_global_context(seen)
                seen.add(eprev.node)

    @property
    def global_context(self) -> Iterable[Definition]:
        """All of the definitions in the global context when this definition was created.

        Note that this does not include this definition itself, except when the definition is a inductive,
        constructor or projection. Because those are mutually recursive objects, they reference themselves
        and are therefore part of their own global context.
        """
        yield from self.cluster_representative.__global_context(set())

    @property
    def clustered_global_context(self) -> Iterable[list[Definition]]:
        """All of the definitions in the global context when this definition was created, clustered into
        mutually recursive cliques.

        The definition itself may be part of the first mutually recursive cluster."""
        yield from self.cluster_representative.__clustered_global_context(set())

    @dataclass
    class Original: pass
    @dataclass
    class Discharged:
        original: Definition
    @dataclass
    class Substituted:
        original: Definition
    @property
    def status(self) -> Original | Discharged | Substituted:
        """A definition is either
        (1) an object as originally inputted by the user.
        (2) a definition that was originally defined in a section and has now had the section
            variables discharged into it.
        (3) a definition that was obtained by performing some sort of module functor substitution.
        When a definition is not original, we cross-reference to the definition that it was derived from.
        """
        status = self.__reader.status
        match status.which:
            case graph_api_capnp.Definition.Status.original:
                return Definition.Original()
            case graph_api_capnp.Definition.Status.discharged:
                return Definition.Discharged(
                    cast(Definition, Node(self.__graph, status.discharged,
                                          self.__lreader).definition))
            case graph_api_capnp.Definition.Status.substituted:
                return Definition.Substituted(
                    cast(Definition,
                         Node(self.__lreader.local_to_global(self.__graph, status.substituted.depIndex),
                              status.substituted.nodeIndex, self.__lreader).definition))
            case _:
                assert False

    @dataclass
    class Inductive:
        representatative: Definition
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
        kind = self.__reader
        graph = self.__graph
        lreader = self.__lreader
        class Seq(Sequence[ProofStep]):
            def __init__(self, reader):
                self.__reader = reader
                self.__count = len(reader)
            def __getitem__(self, index: int) -> ProofStep:
                if index >= self.__count:
                    raise IndexError()
                return ProofStep(self.__reader[index], graph, lreader)
            def __len__(self) -> int:
                return self.__count
        # TODO: pycapnp does not seem to allow matching on graph_api_capnp.Definition.inductive,
        #       we should report this at some point. Alternative is to use the string.
        match kind.which:
            case "inductive":
                return Definition.Inductive(
                    cast(Definition, Node(self.__graph, kind.inductive,
                                          self.__lreader).definition))
            case "constructor":
                return Definition.Inductive(
                    cast(Definition, Node(self.__graph, kind.constructor,
                                          self.__lreader).definition))
            case "projection":
                return Definition.Projection(
                    cast(Definition, Node(self.__graph, kind.projection,
                                          self.__lreader).definition))
            case "manualConstant":
                return Definition.ManualConstant()
            case "tacticalConstant":
                return Definition.TacticalConstant(Seq(kind.tacticalConstant))
            case "manualSectionConstant":
                return Definition.ManualConstant()
            case "tacticalSectionConstant":
                return Definition.TacticalSectionConstant(Seq(kind.tacticalSectionConstant))
            case _:
                assert False

    @property
    def proof(self) -> Sequence[ProofStep] | None:
        """An optional tactical proof that was used to create this definition."""
        match self.kind:
            case Definition.TacticalConstant(proof):
                return proof
            case Definition.TacticalSectionConstant(proof):
                return proof
            case _:
                return None

    @property
    def cluster_representative(self) -> Definition:
        """A unique representative of the mutually recursive cluster of definitions this definition is part of.
        If the definition is not mutually recursive, the representative is itself.

        This is a low-level property. Prefer to use the `cluster` property.
        """
        match self.kind:
            case Definition.Inductive(representative):
                return representative
            case Definition.Constructor(representative):
                return representative
            case Definition.Projection(representative):
                return representative
            case _:
                return self

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
        return self.__reader.typeText

    @property
    def term_text(self) -> str | None:
        """A textual representation of the body of this definition.
        For inductives, constructors, projections, section variables and axioms this is `None`.
        """
        text = self.__reader.termText
        if not text:
            return None
        else:
            return text

class Dataset:
    """The data corresponding to a single Coq source file. The data contains a representation of all definitions
    that have existed at any point throughout the compilation of the source file.
    """

    filename: Path
    """The physical file in which the data contained in this class can be found."""

    def __init__(self, filename: Path, graph: GraphId, lreader: LowlevelDataReader):
        self.filename = filename
        self.__graph = graph
        self.__reader = lreader[graph]
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def dependencies(self) -> Sequence[Path]:
        """A list of physical paths of data files that are direct dependencies of this file."""
        dependencies = self.__reader.dependencies
        # The first dependency is the file itself, which we do not want to expose here.
        count = len(dependencies) - 1
        class Seq(Sequence[Path]):
            def __getitem__(self, index: int) -> Path:
                if index >= count:
                    raise IndexError()
                return Path(dependencies[index+1])
            def __len__(self) -> int:
                return count
        return Seq()

    @property
    def representative(self) -> Definition | None:
        """The entry point of the global context of definitions that are available when this file is 'Required' by
        another file. The full global context can be obtained by following the `previous` node of definitions.
        If the compilation unit does not contain any 'super'-global definitions this may be `None`.

        This is a low-level property. Prefer to use `super_global_context` and `clustered_super_global_context`.
        """
        representative = self.__reader.representative
        if len(self.__reader.graph.nodes) == representative:
            return None
        else:
            return Node(self.__graph, representative, self.__lreader).definition

    @property
    def super_global_context(self) -> Iterable[Definition]:
        """The a list of definitions that become available when this file is `Require`'d by another file, including
        definitions from other files that are recursively `Require`'d."""
        if r := self.representative:
            yield r
            yield from r.__global_context(set())

    @property
    def clustered_super_global_context(self) -> Iterable[list[Definition]]:
        """The list of definitions that become available when this file is `Require`'d by another file, including
        definitions from other files that are recursively `Require`'d. This is clustered by mutually recursive
        definitions."""
        if r := self.representative:
            cluster = list(r.cluster)
            yield cluster
            yield from cluster[-1].__clustered_global_context(set())

    @property
    def definitions(self) -> Sequence[Definition]:
        """All of the definitions present in the file.
        Note that some of these nodes may not be part of the 'super-global' context. Those are definitions inside
        of sections or module functors.
        """
        graph = self.__graph
        lreader = self.__lreader
        ds = self.__reader.definitions
        count = len(ds)
        class Seq(Sequence[Definition]):
            def __getitem__(self, index: int) -> Definition:
                if index >= count:
                    raise IndexError()
                return cast(Definition, Node(graph, ds[index], lreader).definition)
            def __len__(self) -> int:
                return count
        return Seq()

    @property
    def clustered_definitions(self) -> Iterable[list[Definition]]:
        """All of the definitions present in the file, clustered by mutually recursive definitions.
        Note that some of these nodes may not be part of the 'super-global' context. Those are definitions inside
        of sections or module functors.
        """
        graph = self.__graph
        lreader = self.__lreader
        seen = set()
        for nodeid in self.__reader.definitions:
            if not nodeid in seen:
                d = cast(Definition, Node(graph, nodeid, lreader).definition)
                cluster = list(d.cluster)
                yield cluster
                for cd in cluster:
                    seen.add(cd.node.nodeid)

    def node_by_id(self, nodeid: NodeId) -> Node:
        """Lookup a node inside of this file by it's local node-id. This is a low-level function."""
        return Node(self.__graph, nodeid, self.__lreader)

@contextmanager
def data_reader(dataset_path: Path) -> Generator[dict[Path, Dataset], None, None]:
    """Load a directory of dataset files into memory, and expose the data they contain.
    The result is a dictionary that maps physical paths to `Dataset` instances that allow access to the data.
    """
    with lowlevel_data_reader(dataset_path) as lreader:
        yield {f: Dataset(f, g, lreader)
               for f, g in lreader.graphs_by_filename.items()}

