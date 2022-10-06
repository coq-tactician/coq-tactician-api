import os
from tqdm import tqdm
import graphviz

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

class CapnpFileReader:
    def __init__(self, fname, graph_api_capnp):
        self.fname = fname
        self.graph_api_capnp = graph_api_capnp
    def __enter__(self):
        self.f = open(self.fname)
        self.g = self.graph_api_capnp.Dataset.read(self.f, traversal_limit_in_words=2**64-1)
        return self.g
    def __exit__(self, *exception_data):
        self.g.total_size
        self.f.close()

class GraphVisualisationBrowser:
    def __init__(self, dataset_path, url_prefix):
        self.dataset_path = os.path.abspath(dataset_path)
        self.url_prefix = url_prefix

        self.graph_api_capnp = capnp.load(graph_api_capnp())

        #fnames = [f for f in Path(dataset_path).glob('**/*.bin') if f.is_file()]
        self.fnames = list(local_paths_to_bin_files(dataset_path))
        self.fnames_set = set(self.fnames)
        self.graphs = {}
        self.deps = {}
        self.definitions = {}
        self.representative = {}

        print("Preload graphs and dependencies")
        #for fname in tqdm(self.fnames):
        for fname in self.fnames:
            with self.open_data(fname) as g:
                assert fname == g.dependencies[0], (fname, g.dependencies[0])
                self.deps[fname] = list(g.dependencies)
                self.graphs[fname] = g.graph
                self.definitions[fname] = g.definitions
                self.representative[fname] = g.representative
                g.total_size

        for fname, deps in self.deps.items():
            for dep in deps:
                assert dep in self.deps, (fname, dep)

    def open_data(self, fname):
        assert fname in self.fnames_set # security :-)
        return CapnpFileReader(
            os.path.join(self.dataset_path, fname),
            self.graph_api_capnp
        )

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
        edges = list(graphs[0].edges)

        dot = graphviz.Digraph(format='svg')
        dot.attr('graph', ordering="out")
        dot.attr('graph', label=f"Global context for {fname}")
        dot.attr('graph', fontsize="40pt")
        dot.attr('graph', labelloc="t")
        dot.attr('graph', URL=self.root_file_url())
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
        edges = list(graphs[0].edges)
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

        trans_deps = {}
        sparse_deps = {}

        def calc_trans_deps(f):
            if f in trans_deps:
                return
            for d in self.deps[f][1:]:
                calc_trans_deps(d)
            agg = [trans_deps[d] for d in self.deps[f][1:]]
            trans_deps[f] = set().union(*agg).union(set(self.deps[f][1:]))
        for f in self.deps:
            calc_trans_deps(f)
        def calc_sparse_deps(f):
            res = []
            for d in self.deps[f][1:]:
                found = False
                for d2 in self.deps[f][1:]:
                    if d in trans_deps[d2]:
                        found = True
                if not found:
                    res.append(d)
            sparse_deps[f] = res
        for f in self.deps:
            calc_sparse_deps(f)

        dot = graphviz.Digraph(engine='dot', format='svg')
        dot.attr('graph', ordering="out")
        dot.attr('graph', nodesep="1")
        dot.attr('graph', ranksep="2")
        dot.attr(compound='true')

        hierarchy = {'files': [], 'subdirs': {}}
        for f in sparse_deps:
            dirs = f.split('/')[:-1]
            leaf = hierarchy
            for d in dirs:
                leaf['subdirs'].setdefault(d, {'files': [], 'subdirs': {}})
                leaf = leaf['subdirs'][d]
            leaf['files'].append(f)
        def show_hierarchy(dot, h, path, depth):
            for f in h['files']:
                dot.node(f, f, URL = self.global_context_url(f))
            for d in h['subdirs']:
                cluster_name = 'cluster_' + path + '_' + d
                with dot.subgraph(name=cluster_name) as dot2:
                    dot2.attr('graph', label=d)
                    show_hierarchy(dot2, h['subdirs'][d], path + '_' + d, depth + 1)
        def flat_hierarchy():
            for f in self.deps:
                dot.node(f, f, URL = self.global_context_url(f))
        def tunnel_hierarchy(dot, h, depth):
            print(expand_path)
            for f in h['files']:
                dot.node(f, f, URL = self.global_context_url(f))
                if depth == len(expand_path): tunnel_nodes.append(f)
            for d in h['subdirs']:
                cur_path = expand_path[:depth]+[d]
                node_path = '_'.join(cur_path)
                file_path = '/'.join(cur_path)
                if depth < len(expand_path) and d == expand_path[depth]:
                    cluster_name = 'cluster_' + node_path
                    with dot.subgraph(name=cluster_name) as dot2:
                        dot2.attr('graph', label=d)
                        tunnel_hierarchy(dot2, h['subdirs'][d], depth+1)
                else:
                    dot.node(file_path, file_path, shape = "box", URL = self.directory_url(file_path))
                    if depth == len(expand_path): tunnel_nodes.append(node_path)

        def tunnel_simplification(path):
            dirs = path.split('/')
            i = 0
            while i < len(dirs) and i < len(expand_path) and dirs[i] == expand_path[i]:
                i += 1
            res = '/'.join(dirs[:i+1])
            return res

        #flat_hierarchy()
        use_tunnel = True
        if use_tunnel:
            tunnel_nodes = []
            tunnel_hierarchy(dot, hierarchy, 0)
        else:
            show_hierarchy(dot, hierarchy, 'root', 0)

        used_edges = set()
        if use_tunnel:
            for f, deps in sparse_deps.items():
                f = tunnel_simplification(f)
                for d in deps:
                    d = tunnel_simplification(d)
                    if f == d: continue
                    edge = f,d
                    if edge in used_edges: continue
                    used_edges.add(edge)
                    dot.edge(f,d)
        else:
            for f, deps in sparse_deps.items():
                dirs_f = f.split('/')
                for d in deps:
                    dirs_d = d.split('/')
                    i = 0
                    while i < len(dirs_f) and i < len(dirs_d) and dirs_f[i] == dirs_d[i]:
                        i += 1
                    i += 1

                    ltail = "cluster_root_"+'_'.join(dirs_f[:i])
                    lhead = "cluster_root_"+'_'.join(dirs_d[:i])
                    edge = (ltail, lhead)
                    if edge in used_edges: continue
                    used_edges.add(edge)
                    if i >= len(dirs_f): ltail = None
                    if i >= len(dirs_d): lhead = None
                    dot.edge(f, d, lhead = lhead, ltail = ltail)

        #print(dot)
        return dot.pipe()