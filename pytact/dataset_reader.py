from __future__ import annotations
import mmap
from contextlib import contextmanager
from contextlib import ExitStack
from dataclasses import dataclass
from typing import Any, Generator, Iterable, TypeAlias, Union, cast
from pathlib import Path
import capnp
from pytact.common import graph_api_capnp

capnp.remove_import_hook()  # pyright: ignore
graph_api_capnp = capnp.load(graph_api_capnp())  # pyright: ignore

@contextmanager
def file_dataset_view(fname: Path) -> Generator[Any, None, None]:
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


class LowlevelDataViewer:

    graphs_by_filename: dict[Path, int]
    "Map from filenames in the dataset to the index of that graph"

    graph_files: list[Path]

    def __init__(self, dataset: list[tuple[Path, Any]]):
        self.__graphs = [g for _, g in dataset]
        self.graphs_by_filename = {f: i for i, (f, _) in enumerate(dataset)}
        self.graph_files = [f for f, _ in dataset]
        self.__local_to_global = [[self.graphs_by_filename[Path(f)] for f in g.dependencies] for _, g in dataset]

    def local_to_global(self, graph: int, dep_index: int) -> int:
        return self.__local_to_global[graph][dep_index]

    def __getitem__(self, graph):
        return self.__graphs[graph]

@contextmanager
def lowlevel_data_viewer(dataset_path: Path) -> Generator[LowlevelDataViewer,
                                                          None, None]:
    fnames = [f for f in dataset_path.glob('**/*.bin') if f.is_file()]
    with ExitStack() as stack:
        dataset = [(fname.relative_to(dataset_path),
                    stack.enter_context(file_dataset_view(fname)))
                   for fname in fnames]
        yield LowlevelDataViewer(dataset)


ProofStateId = int
TacticId = int
GraphId = int
NodeId = int
NodeHash = int


class Node:

    nodeid: NodeId

    def __init__(self, graph: GraphId, nodeid: NodeId, lviewer: LowlevelDataViewer):
        self.__lviewer = lviewer
        self.__graph = graph
        self.nodeid = nodeid
        self.__node = lviewer[graph].graph.nodes[nodeid]

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
    def children(self) -> Iterable[tuple[Any, Node]]:
        node = self.__node
        lviewer = self.__lviewer
        edges = lviewer[self.__graph].graph.edges
        for i in range(node.childrenIndex, node.childrenIndex+node.childrenCount):
            edge = edges[i]
            yield (edge.label,
                   Node(lviewer.local_to_global(self.__graph, edge.target.depIndex),
                        edge.target.nodeIndex, lviewer))

    @property
    def definition(self) -> Definition | None:
        if graph_api_capnp.Graph.Node.Label.definition == self.label.which:
            return Definition(self, self.label.definition, self.__graph, self.__lviewer)
        else:
            return None

    @property
    def path(self) -> Path:
        return Path(self.__lviewer[self.__graph].dependencies[0])

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

    def __init__(self, reader, graph: GraphId, lviewer):
        self.__reader = reader
        self.__graph = graph
        self.__lviewer = lviewer

    def __repr__(self):
        return repr(self.__reader)

    @property
    def root(self) -> Node:
        root = self.__reader.root
        lviewer = self.__lviewer
        return Node(lviewer.local_to_global(self.__graph, root.depIndex),
                    root.nodeIndex, self.__lviewer)

    @property
    def context(self) -> Iterable[Node]:
        graph = self.__graph
        lviewer = self.__lviewer
        for id in self.__reader.context:
            yield Node(lviewer.local_to_global(graph, id.depIndex),
                       id.nodeIndex, lviewer)

    @property
    def context_names(self) -> Iterable[str]:
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

    def __init__(self, reader, graph: GraphId, lviewer):
        self.__reader = reader
        self.__graph = graph
        self.__lviewer = lviewer

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def before(self) -> ProofState:
        return ProofState(self.__reader.before, self.__graph, self.__lviewer)

    @property
    def after(self) -> Iterable[ProofState]:
        graph = self.__graph
        lviewer = self.__lviewer
        for after in self.__reader.after:
            yield ProofState(after, graph, lviewer)

    @property
    def term(self) -> Node:
        term = self.__reader.term
        return Node(self.__lviewer.local_to_global(self.__graph, term.depIndex),
                    self.__reader.term.nodeIndex, self.__lviewer)

    @property
    def term_text(self) -> str:
        return self.__reader.termText

    @property
    def tactic_arguments(self) -> Iterable[Node | Unresolvable]:
        graph = self.__graph
        lviewer = self.__lviewer
        for arg in self.__reader.tactic_arguments:
            match arg.which:
                case graph_api_capnp.Argument.unresolvable:
                    yield None
                case graph_api_capnp.Argument.term:
                    yield Node(lviewer.local_to_global(graph, arg.term.depIndex),
                               arg.term.nodeIndex, lviewer)

Unknown: TypeAlias = None
class ProofStep:

    def __init__(self, reader, graph: GraphId, lviewer):
        self.__reader = reader
        self.__graph = graph
        self.__lviewer = lviewer

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
    def outcomes(self) -> Iterable[Outcome]:
        graph = self.__graph
        lviewer = self.__lviewer
        for outcome in self.__reader.outcomes:
            yield Outcome(outcome, graph, lviewer)

class Definition:

    node: Node

    def __init__(self, node: Node, reader, graph: GraphId, lviewer):
        self.node = node
        self.__reader = reader
        self.__graph = graph
        self.__lviewer = lviewer

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
        if len(self.__lviewer[self.__graph].graph.nodes) == nodeid:
            return None
        else:
            node = Node(self.__graph, nodeid, self.__lviewer)
            return node.definition

    @property
    def external_previous(self) -> Iterable[Definition]:
        lviewer = self.__lviewer
        graph = self.__graph
        def representative(dep_index) -> Definition:
            depgraph = lviewer.local_to_global(graph, dep_index)
            return cast(Definition, Node(depgraph, lviewer[depgraph].representative, lviewer).definition)
        for dep_index in self.__reader.externalPrevious:
            yield representative(dep_index)

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
                                          self.__lviewer).definition))
            case graph_api_capnp.Definition.Status.substituted:
                return Definition.Substituted(
                    cast(Definition,
                         Node(self.__lviewer.local_to_global(self.__graph, status.substituted.depIndex),
                              status.substituted.nodeIndex, self.__lviewer).definition))
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
        proof: Iterable[ProofStep]
    @dataclass
    class ManualSectionConstant: pass
    @dataclass
    class TacticalSectionConstant:
        proof: Iterable[ProofStep]
    @property
    def kind(self) -> Union[Inductive, Constructor, Projection,
                            ManualConstant, TacticalConstant,
                            ManualSectionConstant, TacticalSectionConstant]:
        kind = self.__reader
        graph = self.__graph
        lviewer = self.__lviewer
        def make_proof_iterable(reader) -> Iterable[ProofStep]:
            for ps in reader:
                yield ProofStep(ps, graph, lviewer)
        # TODO: pycapnp does not seem to allow matching on graph_api_capnp.Definition.inductive,
        #       we should report this at some point. Alternative is to use the string.
        match kind.which:
            case "inductive":
                return Definition.Inductive(
                    cast(Definition, Node(self.__graph, kind.inductive,
                                          self.__lviewer).definition))
            case "constructor":
                return Definition.Inductive(
                    cast(Definition, Node(self.__graph, kind.constructor,
                                          self.__lviewer).definition))
            case "projection":
                return Definition.Projection(
                    cast(Definition, Node(self.__graph, kind.projection,
                                          self.__lviewer).definition))
            case "manualConstant":
                return Definition.ManualConstant()
            case "tacticalConstant":
                return Definition.TacticalConstant(make_proof_iterable(kind.tacticalConstant))
            case "manualSectionConstant":
                return Definition.ManualConstant()
            case "tacticalSectionConstant":
                return Definition.TacticalSectionConstant(make_proof_iterable(kind.tacticalSectionConstant))
            case _:
                assert False

    @property
    def proof(self) -> Iterable[ProofStep] | None:
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

    # TODO: Change to list[Path]
    dependencies: list[Dataset]

    def __init__(self, filename: Path, dependencies: list[Dataset], graph: GraphId, lviewer):
        self.filename = filename
        self.dependencies = dependencies
        self.__graph = graph
        self.__reader = lviewer[graph]
        self.__lviewer = lviewer

    def __repr__(self):
        return repr(self.__reader)

    def __str__(self):
        return str(self.__reader)

    @property
    def representative(self) -> Definition | None:
        representative = self.__reader.representative
        if len(self.__reader.graph.nodes) == representative:
            return None
        else:
            return Node(self.__graph, representative, self.__lviewer).definition

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
    def definitions(self) -> Iterable[Definition]:
        graph = self.__graph
        lviewer = self.__lviewer
        for nodeid in self.__reader.definitions:
            yield cast(Definition, Node(graph, nodeid, lviewer).definition)

    @property
    def clustered_definitions(self) -> Iterable[list[Definition]]:
        graph = self.__graph
        lviewer = self.__lviewer
        seen = set()
        for nodeid in self.__reader.definitions:
            if not nodeid in seen:
                d = cast(Definition, Node(graph, nodeid, lviewer).definition)
                cluster = list(d.cluster)
                yield cluster
                for cd in cluster:
                    seen.add(cd.node.nodeid)

    def node_by_id(self, nodeid: NodeId) -> Node:
        return Node(self.__graph, nodeid, self.__lviewer)

@contextmanager
def data_viewer(dataset_path: Path) -> Generator[dict[Path, Dataset], None, None]:
    with lowlevel_data_viewer(dataset_path) as lviewer:
        datasets = {}
        def populate(path: Path):
            if not path in datasets:
                graph = lviewer.graphs_by_filename[path]
                reader = lviewer[graph]
                deps = [Path(dep) for dep in list(reader.dependencies)[1:]]
                for dep in deps:
                    populate(dep)
                datasets[path] = Dataset(path, [datasets[dep] for dep in deps], graph, lviewer)

        for path in lviewer.graphs_by_filename:
            populate(path)
        yield datasets
