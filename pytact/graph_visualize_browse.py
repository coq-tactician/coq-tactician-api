import os
import graphviz
from collections import defaultdict

from pytact.graph_visualize import edge_arrow_map

from pathlib import Path
from pytact.dataset_reader import Dataset, Definition, Node

class GraphVisualisationBrowser:
    def __init__(self, dataset: dict[Path, Dataset], url_prefix: str):
        self.url_prefix = url_prefix
        self.dataset = dataset

    def get_url(self, bin_fname: Path, svg_fname):
        base = bin_fname.with_suffix('')
        return self.url_prefix+str(base/svg_fname)
    def definition_url(self, fname: Path, defid: int):
        return self.get_url(fname, f"definition-{defid}.svg")
    def proof_url(self, fname: Path, defid: int):
        return self.get_url(fname, f"definition-{defid}-proof.svg")
    def proof_outcome_url(self, fname: Path, defid: int, stepi: int, outcomei: int):
        return self.get_url(fname, f"definition-{defid}-proof-step-{stepi}-outcome-{outcomei}.svg")
    def global_context_url(self, fname: Path):
        return self.get_url(fname, "index.svg")
    def directory_url(self, path: Path):
        return self.url_prefix+str(path/"file_deps.svg")
    def root_file_url(self):
        return self.directory_url(Path())

    def render_node(self, dot, node: Node, label_prefix=None, prefix=None):
        id = repr(node)
        if prefix:
            id = prefix + '-' + id
        label = node.label.which()
        url = None
        if d := node.definition:
            label = d.name
            url = self.definition_url(node.path, node.nodeid)
        if label_prefix:
            label = label_prefix + label
        dot.node(id, label, URL = url)
        return id

    def render_file_node(self, dot, f: Path):
        label = f"File:{f}"
        node_id = f"file-{f}"
        dot.node(node_id, label, URL = self.global_context_url(f))
        return node_id

    def global_context(self, fname: Path):
        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        dot.attr('graph', label=f"Global context for {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.directory_url(Path(*fname.parts[:-1])))

        dataset = self.dataset[fname]
        representative = dataset.representative
        for d in dataset.definitions:
            label_prefix = None
            if representative and representative.node == d.node:
                label_prefix = "Representative: "
            match d.status:
                case Definition.Original():
                    id = self.render_node(dot, d.node, label_prefix=label_prefix)
                case Definition.Discharged(target):
                    id = self.render_node(dot, d.node, label_prefix=label_prefix)
                    dot.edge(id, repr(target.node),
                             arrowtail="inv", dir="both", constraint="false", style="dashed")
                case Definition.Substituted(target):
                    if d.node.path == target.node.path:
                        id = self.render_node(dot, d.node, label_prefix=label_prefix)
                        dot.edge(id, str(target.node),
                                 arrowtail="odot", dir="both", constraint="false", style="dashed")
                    else:
                        with dot.subgraph() as dot2:
                            dot2.attr(rank='same')
                            id = self.render_node(dot2, d.node, label_prefix=label_prefix)
                            id2 = self.render_node(dot2, target.node)
                            dot2.edge(id, id2,
                                      arrowtail="odot", dir="both", constraint="false", style="dashed")

            if prev := d.previous:
                dot.edge(id, repr(prev.node),
                         arrowtail="dot", dir="both", constraint="true")
            for fi in d.external_previous:
                fid = self.render_file_node(dot, fi.node.path)
                dot.edge(id, fid,
                         arrowtail="dot", dir="both", constraint="true")

        print(dot)
        return dot.pipe()

    def visualize_term(self, dot, start: Node, depth,
                       prefix = None, maxNodes = 100, showLabel=False, seen=set()):
        nodes_left = maxNodes
        def recurse(node: Node, depth):
            nonlocal seen
            nonlocal nodes_left

            id = str(node)
            if prefix:
                id = prefix + '-' + id
            if id in seen:
                return id
            else:
                seen.add(id)
            nodes_left -= 1
            if nodes_left < 0:
                id = 'trunc' + str(nodes_left)
                dot.node(id, 'truncated')
                return id

            id = self.render_node(dot, node, prefix=prefix)
            if node.definition:
                depth -= 1
            if depth >= 0:
                for edge, child in node.children:
                    if edge != 'constOpaqueDef':
                        cid = recurse(child, depth)
                        if showLabel:
                            label = str(edge)
                        else:
                            label = ""
                        dot.edge(id, cid, label=label,
                                 arrowtail=edge_arrow_map[edge], dir="both")
            return id
        recurse(start, depth)
        return seen

    def definition(self, fname: Path, definition: int, showLabel=False, depth=1, maxNodes=100):

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")

        start = self.dataset[fname].node_by_id(definition)

        self.visualize_term(dot, start, depth=depth,
                            prefix=None, maxNodes=maxNodes, showLabel=showLabel, seen=set())

        label = "[not a definition]"
        if d := start.definition:
            label = d.name
            match d.kind:
                case Definition.TacticalConstant(proof):
                    dot.node('proof_reference', "Proof",
                             URL = self.proof_url(fname, definition),
                             fontsize="40pt")
                case Definition.TacticalSectionConstant(proof):
                    dot.node('proof_reference', "Proof",
                             URL = self.proof_url(fname, definition),
                             fontsize="40pt")
                case _:
                    pass

        dot.attr('graph', label=f"Definition {label} from {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.global_context_url(fname))
        return dot.pipe()

    def proof(self, fname: Path, definition: int):
        node = self.dataset[fname].node_by_id(definition)
        d = node.definition
        if not d:
            assert False
        match d.kind:
            case Definition.TacticalConstant(proof):
                proof = proof
            case Definition.TacticalConstant(proof):
                proof = proof
            case _:
                assert False

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        surrogates = set()
        outcome_to_id = {}
        for i, step in enumerate(proof):
            for j, outcome in enumerate(step.outcomes):
                id = str(outcome.before.id)
                while id in surrogates:
                    id = id + '-s'
                surrogates.add(id)
                outcome_to_id[(i, j)] = id
        for i, step in enumerate(proof):
            with dot.subgraph(name='cluster_' + str(i)) as dot2:
                dot2.attr('graph', labelloc="b")
                if tactic := step.tactic:
                    tactic_text = tactic.text
                else:
                    tactic_text = 'unknown'
                dot2.attr(label=tactic_text)
                for j, outcome in enumerate(step.outcomes):
                    before_id = outcome_to_id[(i, j)]
                    dot2.node(before_id, label='⬤', shape='circle',
                              fontsize="10pt", fixedsize="true", width="0.3pt",
                              style="filled", fillcolor="white",
                              URL = self.proof_outcome_url(fname, definition, i, j))
                    for after in outcome.after:
                        if outcome.before.id == after.id:
                            after_id = before_id + '-s'
                            style = 'dashed'
                        else:
                            after_id = str(after.id)
                            style = 'solid'
                        dot.edge(before_id, after_id, style=style)
                    if not outcome.after:
                        qedid = str('qed-'+str(i)+'-'+str(j))
                        dot2.node(qedid, label='', shape='point')
                        dot.edge(before_id, qedid)

        dot.attr('graph', label=f"Proof of {d.name} from {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.definition_url(fname, definition))
        #print(dot.source)
        return dot.pipe()

    def outcome(self, fname: Path, definition: int, stepi: int, outcomei: int,
                      depth = 0, maxNodes=100, showLabel=False):
        node = self.dataset[fname].node_by_id(definition)
        d = node.definition
        if not d:
            assert False
        match d.kind:
            case Definition.TacticalConstant(proof):
                proof = proof
            case Definition.TacticalConstant(proof):
                proof = proof
            case _:
                assert False

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")

        outcome = proof[stepi].outcomes[outcomei]
        seen = set()

        with dot.subgraph(name='cluster_before') as dot2:
            dot2.attr('graph', label="Before state")
            self.visualize_term(dot2, outcome.before.root, depth=depth, prefix='before',
                                maxNodes=maxNodes, showLabel=showLabel, seen=seen)

        for ai, after in enumerate(outcome.after):
            with dot.subgraph(name='cluster_after' + str(ai)) as dot2:
                dot2.attr('graph', label="After state " + str(ai))
                self.visualize_term(dot2, after.root, depth=depth, prefix='after'+str(ai),
                                    maxNodes=maxNodes, showLabel=showLabel, seen=seen)

        with dot.subgraph(name='cluster_term') as dot2:
            dot2.attr('graph', label="Proof term")
            prefix = 'term'
            self.visualize_term(dot2, outcome.term, depth=depth,
                                prefix=prefix, maxNodes=maxNodes, showLabel=False, seen=seen)
            # Sometimes the subgraph is completely empty because the term is contained in another subgraph.
            # Therefore, we artificially add a extra root node
            dot2.node('artificial-root', 'artificial-root')
            id = prefix + "-" + str(outcome.term)
            dot2.edge('artificial-root', id)


        dot.attr('graph', label="Proof step " + str(stepi) + " outcome " + str(outcomei) +
                 " of " + d.name + " from " + str(fname))
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.proof_url(fname, definition))

        return dot.pipe('svg')

    def file_deps(self, expand_path: Path):
        expand_parts = expand_path.parts

        dot = graphviz.Digraph(engine='dot', format='svg')
        dot.attr('graph', ordering="out")

        trans_deps = transitive_closure({d.filename: {dep.filename for dep in d.dependencies}
                                         for d in self.dataset.values()})

        hierarchy = {'files': [], 'subdirs': {}}
        for f in self.dataset:
            dirs = f.parent.parts
            leaf = hierarchy
            for d in dirs:
                leaf['subdirs'].setdefault(d, {'files': [], 'subdirs': {}})
                leaf = leaf['subdirs'][d]
            leaf['files'].append(f)
        def common_length(p1, p2):
            return len(os.path.commonprefix([p1, p2]))
        def retrieve_edges(rel, h, depth: int):
            for n in h['files']:
                deps = {Path(*d.parts[:depth+1]) for d in trans_deps[n]
                        if common_length(d.parts, expand_parts) == depth}
                rel[Path(*n.parts[:depth+1])] |= deps
            for d in h['subdirs']:
                retrieve_edges(rel, h['subdirs'][d], depth)
            return rel
        def tunnel_hierarchy(dot, h, depth):
            if depth == len(expand_parts):
                rel = retrieve_edges(defaultdict(set), h, depth)
                (rel, repr) = transitive_reduction(rel)
                def get_url(r: Path):
                    if r in self.dataset:
                        return self.global_context_url(r)
                    else:
                        return self.directory_url(r)
                for n in rel:
                    if len(repr[n]) == 1:
                        label = n.parts[-1]
                        url = get_url(n)
                        shape = 'box'
                    else:
                        label = '<<table border="0" cellborder="1" cellpadding="7">'
                        for r in repr[n]:
                            label += f'<tr><td href="{get_url(r)}">{r.parts[-1]}</td></tr>'
                        label += "</table>>"
                        url = None
                        shape = 'plaintext'
                    dot.node(str(n), label, URL = url, shape = shape, margin="0,0")
                for n, deps in rel.items():
                    for d in deps:
                        dot.edge(str(n), str(d))
            else:
                for d in h['subdirs']:
                    cur_path: Path = Path(*expand_parts[:depth]) / d
                    if depth < len(expand_parts) and d == expand_parts[depth]:
                        cluster_name = 'cluster_' + str(cur_path)
                        with dot.subgraph(name=cluster_name) as dot2:
                            dot2.attr('graph', style="filled", fillcolor="white", label=d,
                                      URL = self.directory_url(Path(*expand_parts[:depth+1])))
                            tunnel_hierarchy(dot2, h['subdirs'][d], depth+1)

        with dot.subgraph(name='cluster_root') as dot2:
            dot2.attr('graph', style="filled", fillcolor="white", label='dataset',
                      URL = self.root_file_url())
            tunnel_hierarchy(dot2, hierarchy, 0)

        #print(dot)
        return dot.pipe()

def transitive_closure(rel):
    trans_deps = defaultdict(set)
    def calc_trans_deps(a, n):
        for d in rel[n]:
            if d not in trans_deps[a]:
                trans_deps[a].add(d)
                calc_trans_deps(a, d)
    for n in list(rel.keys()):
        calc_trans_deps(n, n)
    return trans_deps

def transitive_reduction(rel):
    """This is not a real transitive reduction (NP-hard when it needs to be a subgraph).
    This is an approximation where all strongly connected components are smashed together
    in one node.
    """
    trans_deps = transitive_closure(rel)
    repr_items = defaultdict(set)
    representative = {}
    def calc_components(n):
        if n in representative:
            return
        repr_items[n].add(n)
        representative[n] = n
        for d in trans_deps[n]:
            if n in trans_deps[d]:
                repr_items[n].add(d)
                representative[d] = n
    for n in list(rel.keys()):
        calc_components(n)
    repr_trans_rel = defaultdict(set)
    def calc_new_trans_deps(n):
        for d in trans_deps[n]:
            if n not in trans_deps[d]:
                repr_trans_rel[n].add(representative[d])
    for n in list(rel.keys()):
        calc_new_trans_deps(n)
    def calc_sparse_deps(n):
        res = set()
        for d in repr_trans_rel[n]:
            k = [d2 for d2 in repr_trans_rel[n]
                 if d != d2 and d in trans_deps[d2]]
            if not k:
                res.add(d)
        return res
    k = {n: calc_sparse_deps(n) for n in repr_items}
    return (k, repr_items)
