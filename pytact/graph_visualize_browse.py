import os
import graphviz
import mmap
from contextlib import contextmanager
from contextlib import ExitStack
from collections import defaultdict

import capnp
capnp.remove_import_hook()
from pytact.common import graph_api_capnp

from pytact.graph_visualize import edge_arrow_map

def local_paths_to_bin_files(global_path, local_path = None):
    if os.path.isdir(global_path):
        for d in os.listdir(global_path):
            global_path2 = os.path.join(global_path, d)
            if local_path is None: local_path2 = d
            else: local_path2 = os.path.join(local_path, d)
            yield from local_paths_to_bin_files(global_path2, local_path2)
    elif os.path.isfile(global_path) and global_path.endswith(".bin"):
        yield local_path

@contextmanager
def fileDatasetView(fname, graph_api_capnp):
    def create_mmap(fname):
        # No need to keep the file open, at least on linux
        # Closing this prevents file descriptor limits from being exceeded
        with open(fname, 'rb') as f:
            return mmap.mmap(f.fileno(), length=0, access=mmap.ACCESS_READ)
    with create_mmap(fname) as mm:
        with memoryview(mm) as mv: # Not entirely clear if we need this, but it also couldn't hurt
            with graph_api_capnp.Dataset.from_bytes(mv, traversal_limit_in_words=2**64-1) as g:
                yield g

@contextmanager
def graphVisualisationBrowser(dataset_path, url_prefix):
    fnames = local_paths_to_bin_files(dataset_path)
    api = capnp.load(graph_api_capnp())
    with ExitStack() as stack:
        dataset = [(fname, stack.enter_context(fileDatasetView(os.path.join(dataset_path, fname), api)))
                   for fname in fnames]
        yield GraphVisualisationBrowser(dataset, url_prefix)

class GraphVisualisationBrowser:
    def __init__(self, dataset, url_prefix):
        self.url_prefix = url_prefix

        self.graphs = {}
        self.deps = {}
        self.definitions = {}
        self.representative = {}

        for fname, g in dataset:
            assert fname == g.dependencies[0], (fname, g.dependencies[0])
            self.deps[fname] = list(g.dependencies)
            self.graphs[fname] = g.graph
            self.definitions[fname] = g.definitions
            self.representative[fname] = g.representative

        for fname, deps in self.deps.items():
            for dep in deps:
                assert dep in self.deps, (fname, dep)

    def get_url(self, bin_fname, svg_fname):
        base = os.path.splitext(bin_fname)[0]
        return self.url_prefix+os.path.join(base, svg_fname)
    def definition_url(self, fname, defid):
        return self.get_url(fname, f"definition-{defid}.svg")
    def proof_url(self, fname, defid):
        return self.get_url(fname, f"definition-{defid}-proof.svg")
    def proof_outcome_url(self, fname, defid, stepi, outcomei):
        return self.get_url(fname, f"definition-{defid}-proof-step-{stepi}-outcome-{outcomei}.svg")
    def global_context_url(self, fname):
        return self.get_url(fname, "index.svg")
    def definitions_dependencies_url(self, fname):
        return self.get_url(fname, "dependencies.svg")
    def directory_url(self, path):
        if path != "" and not path.endswith('/'): path = path+'/'
        return self.url_prefix+path+"file_deps.svg"
    def root_file_url(self):
        return self.directory_url("")

    def node_id(self, depindex, nodeindex):
        return str(depindex) + "-" + str(nodeindex)

    def render_node(self, dot, node, depindex, nodeindex, dependencies, representative, prefix=None):
        id = self.node_id(depindex, nodeindex)
        if node.label.which() == 'definition':
            if prefix:
                id = prefix + '-' + id
            if depindex == 0 and nodeindex == representative:
                label = 'Representative: ' + node.label.definition.name
            else:
                label = node.label.definition.name
            dot.node(id, label, URL = self.definition_url(dependencies[depindex], nodeindex))
        else:
            label = node.label.which()
            dot.node(id, label)
        return id

    def render_file_node(self, dot, deps, i):
        label = f"File:{deps[i]}"
        node_id = f"file-{i}"
        dot.node(node_id, label, URL = self.global_context_url(deps[i]))
        return node_id

    def global_context(self, fname):
        dependencies = self.deps[fname]
        graphs = [self.graphs[dep] for dep in dependencies]
        nodes = graphs[0].nodes

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        dot.attr('graph', label=f"Global context for {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.directory_url('/'.join(fname.split('/')[:-1])))
        dot.node('dependency_graph_reference', "Dependencies between definitions",
                 URL = self.definitions_dependencies_url(fname),
                 fontsize="40pt")
        representative = self.representative[fname]
        for node in self.definitions[fname]:
            value = nodes[node]
            if value.label.definition.status.which () == 'original':
                id = self.render_node(dot, value, 0, node, dependencies, representative)
            elif value.label.definition.status.which () == 'discharged':
                id = self.render_node(dot, value, 0, node, dependencies, representative)
                dot.edge(id, self.node_id(0, value.label.definition.status.discharged),
                         arrowtail="inv", dir="both", constraint="false", style="dashed")
            elif value.label.definition.status.which () == 'substituted':
                target = value.label.definition.status.substituted
                if target.depIndex == 0:
                    id = self.render_node(dot, value, 0, node, dependencies, representative)
                    dot.edge(id, self.node_id(target.depIndex, target.nodeIndex),
                             arrowtail="odot", dir="both", constraint="false", style="dashed")
                else:
                    with dot.subgraph() as dot2:
                        dot2.attr(rank='same')
                        id = self.render_node(dot2, value, 0, node, dependencies, representative)
                        id2 = self.render_node(dot2, graphs[target.depIndex].nodes[target.nodeIndex],
                                            target.depIndex, target.nodeIndex, dependencies, representative)
                        dot2.edge(id, id2,
                                 arrowtail="odot", dir="both", constraint="false", style="dashed")
            if value.label.definition.previous != len(nodes):
                dot.edge(id, self.node_id(0, value.label.definition.previous),
                         arrowtail="dot", dir="both", constraint="true")
            for fi in value.label.definition.externalPrevious:
                fid = self.render_file_node(dot, dependencies, fi)
                dot.edge(id, fid,
                         arrowtail="dot", dir="both", constraint="true")

        return dot.pipe()

    def definition_dependencies(self, fname):
        dependencies = self.deps[fname]
        graphs = [self.graphs[dep] for dep in dependencies]
        nodes = graphs[0].nodes
        edges = graphs[0].edges
        representative = self.representative[fname]

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        dot.attr('graph', ranksep="2")
        dot.attr('graph', rankdir="LR")

        deps = {}
        external_deps = {}
        external_set = set()
        for node in self.definitions[fname]:
            deps[node] = set()
            external_deps[node] = set()
            seen = set()
            def recurse(depi, nodei):
                nonlocal seen
                id = self.node_id(depi, nodei)
                if id in seen:
                    return
                else:
                    seen.add(id)
                if depi != 0:
                    external_deps[node].add((depi, nodei))
                    external_set.add((depi, nodei))
                    return
                else:
                    value = nodes[nodei]
                    if nodei != node and value.label.which() == 'definition':
                        deps[node].add(nodei)
                    elif nodei == node or value.label.which() != 'definition':
                        for edge in edges[value.childrenIndex:value.childrenIndex+value.childrenCount]:
                            recurse(edge.target.depIndex, edge.target.nodeIndex)
            recurse(0, node)

        dot.attr('graph', label=f"Definition dependencies for {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.global_context_url(fname))
        for ni in deps:
            self.render_node(dot, nodes[ni], 0, ni, dependencies, representative)
        for definition, deps in deps.items():
            for d in deps:
                id1 = self.node_id(0, definition)
                id2 = self.node_id(0, d)
                dot.edge(id1, id2)
        for (di, ni) in external_set:
            self.render_node(dot, graphs[di].nodes[ni], di, ni, dependencies, representative)

        for definition, deps in external_deps.items():
            for (di, ni) in deps:
                id1 = self.node_id(0, definition)
                id2 = self.node_id(di, ni)
                dot.edge(id1, id2)

        return dot.pipe()

    def visualize_term(self, dot, graphs, start, dependencies, depth,
                       prefix = None, maxNodes = 100, showLabel=False, seen=set()):
        edges = graphs[0].edges
        nodes_left = maxNodes
        def recurse(depi, nodei, depth):
            nonlocal seen
            nonlocal nodes_left

            value = graphs[depi].nodes[nodei]
            id = self.node_id(depi, nodei)
            if value.label.which() == 'definition' and prefix:
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

            if depi != 0:
                return self.render_node(dot, graphs[depi].nodes[nodei], depi, nodei, dependencies, -1, prefix)
            else:
                id = self.render_node(dot, value, depi, nodei, dependencies, -1, prefix)
                if value.label.which() == 'definition':
                    depth -= 1
                if depth >= 0:
                    for edgei in range(value.childrenIndex, value.childrenIndex+value.childrenCount):
                        edge = edges[edgei]
                        if edge.label != 'constOpaqueDef':
                            cid = recurse(edge.target.depIndex, edge.target.nodeIndex, depth)
                            if showLabel:
                                label = str(edge.label)
                            else:
                                label = ""
                            if str(edge.label) in ["appArgOrder"]:
                                constraint = "false"
                            else:
                                constraint = "true"
                            dot.edge(id, cid, label=label,
                                     arrowtail=edge_arrow_map[edge.label], dir="both", constraint=constraint)
                return id
        recurse(0, start, depth)
        return seen

    def definition(self, fname, definition, showLabel=False, depth=1, maxNodes=100):
        dependencies = self.deps[fname]
        graphs = [self.graphs[dep] for dep in dependencies]

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        label = graphs[0].nodes[definition].label.definition.name
        dot.attr('graph', label=f"Definition {label} from {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.global_context_url(fname))

        self.visualize_term(dot, graphs, definition, dependencies,
                       depth=depth, prefix=None, maxNodes=maxNodes, showLabel=showLabel, seen=set())

        node_value = graphs[0].nodes[definition].label.definition
        if node_value.which() == 'tacticalConstant':
            dot.node('proof_reference', "Proof",
                     URL = self.proof_url(fname, definition),
                     fontsize="40pt")
        elif node_value.which() == 'tacticalSectionConstant':
            dot.node('proof_reference', "Proof",
                     URL = self.proof_url(fname, definition),
                     fontsize="40pt")

        return dot.pipe()

    def proof(self, fname, definition):
        dependencies = self.deps[fname]
        graphs = [self.graphs[dep] for dep in dependencies]
        node_value = graphs[0].nodes[definition].label.definition
        if node_value.which() == 'tacticalConstant':
            proof = node_value.tacticalConstant
        elif node_value.which() == 'tacticalSectionConstant':
            proof = node_value.tacticalSectionConstant

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        label = graphs[0].nodes[definition].label.definition.name
        dot.attr('graph', label=f"Proof of {label} from {dependencies[0]}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.definition_url(fname, definition))
        surrogates = {}
        step_before_ids = {}
        has_after = {}
        for i, step in enumerate(proof):
            for j, outcome in enumerate(step.outcomes):
                id = str(outcome.before.id)
                while id in step_before_ids:
                    id = id + '-s'
                step_before_ids[id] = (i, j)
                for after in outcome.after:
                    has_after[after.id] = True
        for i, step in enumerate(proof):
            with dot.subgraph(name='cluster_' + str(i)) as dot2:
                if step.tactic.which() == 'known':
                    tactic_text = step.tactic.known.text
                else:
                    tactic_text = 'unknown'
                dot2.attr(label=tactic_text)
                for j, outcome in enumerate(step.outcomes):
                    if outcome.before.id not in has_after:
                        #dot.node(str(outcome.before.id), label='⬤', shape='doublecircle',
                        #       URL = self.proof_outcome_url(fname, definition, i, j))
                        dot.node(str(outcome.before.id), label='⬤', shape='doublecircle',
                                 style="filled", fillcolor="white",
                                 URL = self.proof_outcome_url(fname, definition, i, j))

                    for after in outcome.after:
                        if outcome.before.id == after.id:
                            before_id = surrogates.get(str(outcome.before.id), str(outcome.before.id))
                            after_id = before_id + '-s'
                            (stepi, outcomei) = step_before_ids[after_id]
                            dot2.node(after_id, label='⬤', shape='doublecircle',
                                      style="filled", fillcolor="white",
                                      URL = self.proof_outcome_url(fname, definition, stepi, outcomei))
                            dot.edge(before_id, after_id, style='dashed')
                            surrogates[str(after.id)] = after_id
                        else:
                            before_id = surrogates.get(str(outcome.before.id), str(outcome.before.id))
                            after_id = surrogates.get(str(after.id), str(after.id))
                            (stepi, outcomei) = step_before_ids[after_id]
                            dot2.node(after_id, label='⬤', shape='doublecircle',
                                      style="filled", fillcolor="white",
                                      URL = self.proof_outcome_url(fname, definition, stepi, outcomei))
                            dot.edge(before_id, after_id)
                    if not outcome.after:
                        qedid = str('qed-'+str(i)+'-'+str(j))
                        before_id = surrogates.get(str(outcome.before.id), str(outcome.before.id))
                        dot2.node(qedid, label='', shape='point')
                        dot.edge(str(before_id), str(qedid))

        #print(dot.source)
        return dot.pipe()

    def outcome(self, fname, definition, stepi, outcomei,
                      depth = 0, maxNodes=100, showLabel=False):
        dependencies = self.deps[fname]
        graphs = [self.graphs[dep] for dep in dependencies]
        node_value = graphs[0].nodes[definition].label.definition
        if node_value.which() == 'tacticalConstant':
            proof = node_value.tacticalConstant
        elif node_value.which() == 'tacticalSectionConstant':
            proof = node_value.tacticalSectionConstant

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")

        outcome = proof[stepi].outcomes[outcomei]
        seen = set()

        with dot.subgraph(name='cluster_before') as dot2:
            dot2.attr('graph', label="Before state")
            self.visualize_term(dot2, graphs, outcome.before.root, dependencies,
                                depth=depth, prefix='before', maxNodes=maxNodes, showLabel=showLabel,
                                seen=seen)

        for ai, after in enumerate(outcome.after):
            with dot.subgraph(name='cluster_after' + str(ai)) as dot2:
                dot2.attr('graph', label="After state " + str(ai))
                self.visualize_term(dot2, graphs, after.root, dependencies,
                               depth=depth, prefix='after'+str(ai), maxNodes=maxNodes, showLabel=showLabel,
                               seen=seen)

        with dot.subgraph(name='cluster_term') as dot2:
            dot2.attr('graph', label="Proof term")
            prefix = 'term'
            self.visualize_term(dot2, graphs, outcome.term, dependencies,
                           depth=depth, prefix=prefix, maxNodes=maxNodes, showLabel=False,
                           seen=seen)
            # Sometimes the subgraph is completely empty because the term is contained in another subgraph.
            # Therefore, we artificially add a extra root node
            dot2.node('artificial-root', 'artificial-root')
            id = self.node_id(0, outcome.term)
            value = graphs[0].nodes[outcome.term]
            if value.label.which() == 'definition' and prefix:
                id = prefix + '-' + id
            dot2.edge('artificial-root', id)


        label = graphs[0].nodes[definition].label.definition.name
        dot.attr('graph', label="Proof step " + str(stepi) + " outcome " + str(outcomei) +
                 " of " + label + " from " + str(dependencies[0]))
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.proof_url(dependencies[0], definition))

        return dot.pipe('svg')

    def file_deps(self, expand_path):

        dot = graphviz.Digraph(engine='dot', format='svg')
        dot.attr('graph', ordering="out")

        hierarchy = {'files': [], 'subdirs': {}}
        for f in self.deps:
            dirs = f.split('/')[:-1]
            leaf = hierarchy
            for d in dirs:
                leaf['subdirs'].setdefault(d, {'files': [], 'subdirs': {}})
                leaf = leaf['subdirs'][d]
            leaf['files'].append(f)
        def common_length(p1, p2):
            return len(os.path.commonprefix([p1, p2]))
        def retrieve_edges(rel, h, depth):
            for n in h['files']:
                deps = {'/'.join(d.split('/')[:depth+1]) for d in self.deps[n]
                        if common_length(d.split('/'), expand_path) == depth}
                rel['/'.join(n.split('/')[:depth+1])] |= deps
            for d in h['subdirs']:
                retrieve_edges(rel, h['subdirs'][d], depth)
            return rel
        def tunnel_hierarchy(dot, h, depth):
            if depth == len(expand_path):
                rel = retrieve_edges(defaultdict(set), h, depth)
                (rel, repr) = transitive_reduction(rel)
                def get_url(r):
                    if r in self.deps:
                        return self.global_context_url(r)
                    else:
                        return self.directory_url(r)
                for n in rel:
                    if len(repr[n]) == 1:
                        label = n.split("/")[-1]
                        url = get_url(n)
                        shape = 'box'
                    else:
                        label = '<<table border="0" cellborder="1" cellpadding="7">'
                        for r in repr[n]:
                            label += f'<tr><td href="{get_url(r)}">{r.split("/")[-1]}</td></tr>'
                        label += "</table>>"
                        url = None
                        shape = 'plaintext'
                    dot.node(n, label, URL = url, shape = shape, margin="0,0")
                for n, deps in rel.items():
                    for d in deps:
                        dot.edge(n, d)
            else:
                for d in h['subdirs']:
                    cur_path = expand_path[:depth]+[d]
                    file_path = '/'.join(cur_path)
                    if depth < len(expand_path) and d == expand_path[depth]:
                        cluster_name = 'cluster_' + file_path
                        with dot.subgraph(name=cluster_name) as dot2:
                            dot2.attr('graph', style="filled", fillcolor="white", label=d,
                                      URL = self.directory_url('/'.join(expand_path[:depth+1])))
                            tunnel_hierarchy(dot2, h['subdirs'][d], depth+1)

        with dot.subgraph(name='cluster_root') as dot2:
            dot2.attr('graph', style="filled", fillcolor="white", label='dataset',
                      URL = self.directory_url(''))
            tunnel_hierarchy(dot2, hierarchy, 0)

        #print(dot)
        return dot.pipe()

def transitive_reduction(rel):
    """This is not a real transitive reduction (NP-hard when it needs to be a subgraph).
    This is an approximation where all strongly connected components are smashed together
    in one node.
    """
    trans_deps = defaultdict(set)
    def calc_trans_deps(a, n):
        for d in rel[n]:
            if d not in trans_deps[a]:
                trans_deps[a].add(d)
                calc_trans_deps(a, d)
    for n in list(rel.keys()):
        calc_trans_deps(n, n)
    repr_items = defaultdict(set)
    representative = {}
    def calc_components(n):
        if n in representative:
            return
        repr_items[n].add(n)
        for d in trans_deps[n]:
            if n in trans_deps[d]:
                repr_items[n].add(d)
                representative[d] = n
    for n in (list(rel.keys())):
        calc_components(n)
    repr_trans_rel = defaultdict(set)
    def calc_new_trans_deps(n):
        for d in trans_deps[n]:
            if n not in trans_deps[d]:
                repr_trans_rel[n].add(representative[d])
    for n in (list(rel.keys())):
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
