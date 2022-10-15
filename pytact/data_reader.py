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

    graphs_by_filename: dict[Path, int]
    "Map from filenames in the dataset to the index of that graph"

    graph_files: list[Path]

    def __init__(self, dataset_path : Path, dataset: list[tuple[Path, Any]]):
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
        return self.__local_to_global[graph][dep_index]

    def __getitem__(self, graph):
        return self.__graphs[graph]

    def __len__(self) -> int:
        return len(self.__graphs)

@contextmanager
def lowlevel_data_reader(dataset_path: Path) -> Generator[LowlevelDataReader, None, None]:
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

    nodeid: NodeId

    def __init__(self, graph: GraphId, nodeid: NodeId, lreader: LowlevelDataReader):
        self.__lreader = lreader
        self.__graph = graph
        self.nodeid = nodeid
        self.__node = lreader[graph].graph.nodes[nodeid]

    def __repr__(self):
        return f"node-{self.__graph}-{self.nodeid}"

    def __eq__(self, other: Node) -> bool:
        return self.__graph == other.__graph and self.nodeid == other.nodeid

    def __hash__(self):
        return hash((self.__graph, self.nodeid))

    @property
    def label(self) -> Any:
        return self.__node.label

    @property
    def identity(self) -> NodeHash:
        return self.__node.identity

    @property
    def children(self) -> Sequence[tuple[Any, Node]]:
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
        if graph_api_capnp.Graph.Node.Label.definition == self.label.which:
            return Definition(self, self.label.definition, self.__graph, self.__lreader)
        else:
            return None

    @property
    def path(self) -> Path:
        return Path(self.__lreader[self.__graph].dependencies[0])

class Tactic:

    def __init__(self, reader):
        self.__reader = reader

    def __repr__(self) -> str:
        return repr(self.__reader)

    @property
    def ident(self) -> TacticId:
        return self.__reader.ident

    @property
    def text(self) -> str:
        return self.__reader.text

    def __str__(self) -> str:
        return self.__reader.text

    @property
    def base_text(self) -> str:
        return self.__reader.baseText

    @property
    def interm_text(self) -> str:
        return self.__reader.intermText

    @property
    def exact(self) -> bool:
        return self.__reader.exact

class ProofState:

    def __init__(self, reader, graph: GraphId, lreader: LowlevelDataReader):
        self.__reader = reader
        self.__graph = graph
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    @property
    def root(self) -> Node:
        root = self.__reader.root
        lreader = self.__lreader
        return Node(lreader.local_to_global(self.__graph, root.depIndex),
                    root.nodeIndex, self.__lreader)

    @property
    def context(self) -> Sequence[Node]:
        graph = self.__graph
        lreader = self.__lreader
        context = self.__reader.context
        count = len(context)
        class Seq(Sequence[Node]):
            def __getitem__(self, index: int) -> Node:
                if index >= count:
                    raise IndexError()
                n = context[index]
                return Node(lreader.local_to_global(graph, n.depIndex),
                            n.nodeIndex, lreader)
            def __len__(self) -> int:
                return count
        return Seq()

    @property
    def context_names(self) -> Sequence[str]:
        return self.__reader.contextNames

    @property
    def text(self) -> str:
        return self.__reader.text

    def __str__(self) -> str:
        return self.__reader.text

    @property
    def id(self) -> ProofStateId:
        return self.__reader.id

Unresolvable: TypeAlias = None
class Outcome:

    def __init__(self, reader, graph: GraphId, lreader: LowlevelDataReader):
        self.__reader = reader
        self.__graph = graph
        self.__lreader = lreader

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def before(self) -> ProofState:
        return ProofState(self.__reader.before, self.__graph, self.__lreader)

    @property
    def after(self) -> Sequence[ProofState]:
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
        term = self.__reader.term
        return Node(self.__lreader.local_to_global(self.__graph, term.depIndex),
                    self.__reader.term.nodeIndex, self.__lreader)

    @property
    def term_text(self) -> str:
        return self.__reader.termText

    @property
    def tactic_arguments(self) -> Sequence[Node | Unresolvable]:
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

Unknown: TypeAlias = None
class ProofStep:

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
        tactic = self.__reader.tactic
        match tactic.which:
            case graph_api_capnp.ProofStep.Tactic.unknown:
                return None
            case graph_api_capnp.ProofStep.Tactic.known:
                return Tactic(tactic.known)

    @property
    def outcomes(self) -> Sequence[Outcome]:
        graph = self.__graph
        lreader = self.__lreader
        outcomes = self.__reader.outcomes
        count = len(outcomes)
        class Seq(Sequence[Outcome]):
            def __getitem__(self, index: int) -> Outcome:
                if index >= count:
                    raise IndexError()
                return Outcome(outcomes[index], graph, lreader)
            def __len__(self) -> int:
                return count
        return Seq()

class Definition:

    node: Node

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
        return self.__reader.name

    @property
    def previous(self) -> Definition | None:
        nodeid = self.__reader.previous
        if len(self.__lreader[self.__graph].graph.nodes) == nodeid:
            return None
        else:
            node = Node(self.__graph, nodeid, self.__lreader)
            return node.definition

    @property
    def external_previous(self) -> Sequence[Definition]:
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
        yield from self.cluster_representative.__global_context(set())

    @property
    def clustered_global_context(self) -> Iterable[list[Definition]]:
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
        match self.kind:
            case Definition.TacticalConstant(proof):
                return proof
            case Definition.TacticalSectionConstant(proof):
                return proof
            case _:
                return None

    @property
    def cluster_representative(self) -> Definition:
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
        return self.__reader.typeText

    @property
    def term_text(self) -> str | None:
        text = self.__reader.termText
        if not text:
            return None
        else:
            return text

class Dataset:
    filename: Path

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
        dependencies = self.__reader.dependencies
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
        representative = self.__reader.representative
        if len(self.__reader.graph.nodes) == representative:
            return None
        else:
            return Node(self.__graph, representative, self.__lreader).definition

    @property
    def super_global_context(self) -> Iterable[Definition]:
        if r := self.representative:
            yield r
            yield from r.__global_context(set())

    @property
    def clustered_super_global_context(self) -> Iterable[list[Definition]]:
        if r := self.representative:
            cluster = list(r.cluster)
            yield cluster
            yield from cluster[-1].__clustered_global_context(set())

    @property
    def definitions(self) -> Sequence[Definition]:
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
        return Node(self.__graph, nodeid, self.__lreader)

@contextmanager
def data_reader(dataset_path: Path) -> Generator[dict[Path, Dataset], None, None]:
    with lowlevel_data_reader(dataset_path) as lreader:
        yield {f: Dataset(f, g, lreader)
               for f, g in lreader.graphs_by_filename.items()}

