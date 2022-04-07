import os
import sys

import graphviz

# Load the cap'n proto library, and the communication specification in 'graph_api.capnp'
import capnp
capnp.remove_import_hook()

import pytact.common

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

arrow_heads = [ "dot", "inv", "odot", "invdot", "invodot" ]
edge_arrow_map = {}
for group in graph_api_capnp.groupedEdges:
    count = 0
    for sort in group.conflatable:
        edge_arrow_map[sort] = arrow_heads[count]
        count += 1

def visualize(graph, state, showLabel = False, graph1 = None):
    nodes = graph.nodes
    root = state.root
    context = state.context
    assert all(n < len(nodes) for n in context)

    dot = graphviz.Digraph()
    dot.attr('graph', ordering="out")
    for node, value in enumerate(nodes):
        label = value.label.which()
        if node in context: label = str(node) + ': ['+label+']'
        if node == root:
            label = "Root: "+label
        dot.node(str(node), label)
    for node, value in enumerate(nodes):
        for edge in list(graph.edges)[value.childrenIndex:value.childrenIndex+value.childrenCount]:
            if graph1 != None and edge.target.depIndex == 1:
                target = '1#'+str(edge.target.nodeIndex)
                dot.node(target, "Global:" + str(graph1.nodes[edge.target.nodeIndex].label.definition.hash)
                         + str(graph1.nodes[edge.target.nodeIndex].label.definition.name))
            else:
                target = str(edge.target.nodeIndex)
            if showLabel:
                label = str(edge.label)
            else:
                label = ""
            if str(edge.label) in ["appArgOrder"]:
                constraint = "false"
            else:
                constraint = "true"
            dot.edge(str(node), target, label=label,
                     arrowtail=edge_arrow_map[edge.label], dir="both", constraint=constraint)

    dot.render('python_graph', view=False)

def visualize_defs(graph, defs, showLabel = False):
    nodes = graph.nodes
    assert all(n < len(nodes) for n in defs)

    dot = graphviz.Digraph()
    dot.attr('graph', ordering="out")
    for node, value in enumerate(nodes):
        label = value.label.which()
        if node in defs: label = str(node) + ': ' + value.label.definition.name
        dot.node(str(node), label)
    for node, value in enumerate(nodes):
        for edge in list(graph.edges)[value.childrenIndex:value.childrenIndex+value.childrenCount]:
            if showLabel:
                label = str(edge.label)
            else:
                label = ""
            if str(edge.label) in ["appArgOrder"]:
                constraint = "false"
            else:
                constraint = "true"
            dot.edge(str(node), str(edge.target.nodeIndex), label=label,
                     arrowtail=edge_arrow_map[edge.label], dir="both", constraint=constraint)

    dot.render('python_graph', view=False)

def definition_url(file, defid):
    base = os.path.splitext(file)[0]
    return os.path.join(base+'-viz', 'definition-' + str(defid) + '.svg')

def proof_url(file, defid):
    base = os.path.splitext(file)[0]
    return os.path.join(base+'-viz', 'definition-' + str(defid) + '-proof.svg')

def proof_outcome_url(file, defid, stepi, outcomei):
    base = os.path.splitext(file)[0]
    return os.path.join(base+'-viz', 'definition-' + str(defid) + '-proof-step-' +
                        str(stepi) + '-outcome-' + str(outcomei) + '.svg')

def global_context_url(file):
    base = os.path.splitext(file)[0]
    return os.path.join(base+'-viz', 'index.svg')

def definitions_dependencies_url(file):
    base = os.path.splitext(file)[0]
    return os.path.join(base+'-viz', 'dependencies.svg')

def root_file_url(root):
    return os.path.join(root, 'dependencies.svg')

def get_id(depindex, nodeindex):
    return str(depindex) + "-" + str(nodeindex)

def render_node(dot, node, depindex, nodeindex, dependencies, representative, prefix=None):
    id = get_id(depindex, nodeindex)
    if node.label.which() == 'definition':
        if prefix:
            id = prefix + '-' + id
        if depindex == 0 and nodeindex == representative:
            label = 'Representative: ' + node.label.definition.name
        else:
            label = node.label.definition.name
        dot.node(id, label, URL = definition_url(dependencies[depindex], nodeindex))
    else:
        label = node.label.which()
        dot.node(id, label)
    return id

def render_file_node(dot, depindex, dependencies):
    file_label = 'File:' + dependencies[depindex]
    id = 'file-' + str(depindex)
    dot.node(id, file_label, URL = global_context_url(dependencies[depindex]))
    return id

def visualize_global_context(root, graphs, definitions, representative, dependencies):
    nodes = graphs[0].nodes

    dot = graphviz.Digraph(format='svg')
    dot.attr('graph', ordering="out")
    dot.attr('graph', label="Global context for " + str(dependencies[0]))
    dot.attr('graph', fontsize="40pt")
    dot.attr('graph', labelloc="t")
    dot.attr('graph', URL=root_file_url(root))
    dot.node('dependency_graph_reference', "Dependencies between definitions",
             URL = definitions_dependencies_url(dependencies[0]),
             fontsize="40pt")
    for node in definitions:
        value = nodes[node]
        if value.label.definition.status.which () == 'original':
            id = render_node(dot, value, 0, node, dependencies, representative)
        elif value.label.definition.status.which () == 'discharged':
            id = render_node(dot, value, 0, node, dependencies, representative)
            dot.edge(id, get_id(0, value.label.definition.status.discharged),
                     arrowtail="inv", dir="both", constraint="false", style="dashed")
        elif value.label.definition.status.which () == 'substituted':
            target = value.label.definition.status.substituted
            if target.depIndex == 0:
                id = render_node(dot, value, 0, node, dependencies, representative)
                dot.edge(id, get_id(target.depIndex, target.nodeIndex),
                         arrowtail="odot", dir="both", constraint="false", style="dashed")
            else:
                with dot.subgraph() as g:
                    g.attr(rank='same')
                    id = render_node(g, value, 0, node, dependencies, representative)
                    id2 = render_node(g, graphs[target.depIndex].nodes[target.nodeIndex],
                                        target.depIndex, target.nodeIndex, dependencies, representative)
                    g.edge(id, id2,
                             arrowtail="odot", dir="both", constraint="false", style="dashed")
        dot.edge(id, get_id(0, value.label.definition.previous),
                 arrowtail="dot", dir="both", constraint="true")
        for fi in value.label.definition.externalPrevious:
            fid = render_file_node(dot, fi, dependencies)
            dot.edge(id, fid,
                     arrowtail="dot", dir="both", constraint="true")

    file = global_context_url(dependencies[0])
    dot.render(os.path.splitext(file)[0], view=False, cleanup=False)
    return file

def visualize_definition_dependencies(root, graphs, definitions, dependencies, representative):
    nodes = graphs[0].nodes
    edges = list(graphs[0].edges)

    dot = graphviz.Digraph(format='svg')
    dot.attr('graph', ordering="out")
    dot.attr('graph', ranksep="2")
    dot.attr('graph', rankdir="LR")

    deps = {}
    external_deps = {}
    external_set = set()
    for node in definitions:
        deps[node] = set()
        external_deps[node] = set()
        seen = set()
        def recurse(depi, nodei):
            nonlocal seen
            id = get_id(depi, nodei)
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

    # g = graphviz.Digraph(name='cluster_internal_defs')
    g = dot
    g.attr('graph', label="Definition dependencies for " + str(dependencies[0]))
    g.attr('graph', fontsize="40pt")
    g.attr('graph', labelloc="t")
    g.attr('graph', URL=global_context_url(dependencies[0]))
    for ni in deps:
        render_node(g, nodes[ni], 0, ni, dependencies, representative)
    for definition, deps in deps.items():
        for d in deps:
            id1 = get_id(0, definition)
            id2 = get_id(0, d)
            g.edge(id1, id2)
    for (di, ni) in external_set:
        render_node(dot, graphs[di].nodes[ni], di, ni, dependencies, representative)
    #dot.subgraph(g)

    for definition, deps in external_deps.items():
        for (di, ni) in deps:
            id1 = get_id(0, definition)
            id2 = get_id(di, ni)
            dot.edge(id1, id2)

    file = definitions_dependencies_url(dependencies[0])
    dot.render(os.path.splitext(file)[0], view=False, cleanup=False)
    return file

def visualize_term(dot, root, graphs, start, dependencies, depth,
                   prefix = None, maxNodes = 100, showLabel=False, seen=set()):
    edges = graphs[0].edges
    nodes_left = maxNodes
    def recurse(depi, nodei, depth):
        nonlocal seen
        nonlocal nodes_left

        value = graphs[depi].nodes[nodei]
        id = get_id(depi, nodei)
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
            return render_node(dot, graphs[depi].nodes[nodei], depi, nodei, dependencies, -1, prefix)
        else:
            id = render_node(dot, value, depi, nodei, dependencies, -1, prefix)
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

# TODO: This doesn't deal properly with inductives and mutually recursive definitions
def visualize_definition(root, graphs, definition, dependencies, showLabel=False, depth=1, maxNodes=100):
    dot = graphviz.Digraph(format='svg')
    dot.attr('graph', ordering="out")
    label = graphs[0].nodes[definition].label.definition.name
    dot.attr('graph', label="Definition " + label + " from " + str(dependencies[0]))
    dot.attr('graph', fontsize="40pt")
    dot.attr('graph', labelloc="t")
    dot.attr('graph', URL=global_context_url(dependencies[0]))

    visualize_term(dot, root, graphs, definition, dependencies,
                   depth=depth, prefix=None, maxNodes=maxNodes, showLabel=showLabel, seen=set())

    node_value = graphs[0].nodes[definition].label.definition
    if node_value.which() == 'tacticalConstant':
        visualize_proof(root, graphs, definition, node_value.tacticalConstant, dependencies)
        dot.node('proof_reference', "Proof",
                 URL = proof_url(dependencies[0], definition),
                 fontsize="40pt")
    elif node_value.which() == 'tacticalSectionConstant':
        visualize_proof(root, graphs, definition, node_value.tacticalSectionConstant, dependencies)
        dot.node('proof_reference', "Proof",
                 URL = proof_url(dependencies[0], definition),
                 fontsize="40pt")

    file = definition_url(dependencies[0], definition)
    dot.render(os.path.splitext(file)[0], view=False, cleanup=True)

def visualize_proof(root, graphs, definition, proof, dependencies):
    dot = graphviz.Digraph(format='svg')
    dot.attr('graph', ordering="out")
    label = graphs[0].nodes[definition].label.definition.name
    dot.attr('graph', label="Proof of " + label + " from " + str(dependencies[0]))
    dot.attr('graph', fontsize="40pt")
    dot.attr('graph', labelloc="t")
    dot.attr('graph', URL=definition_url(dependencies[0], definition))
    surrogates = {}
    step_before_ids = {}
    has_after = {}
    for i, step in enumerate(proof):
        for j, outcome in enumerate(step.outcomes):
            visualize_outcome(root, graphs, definition, proof, i, j, dependencies)
            id = str(outcome.before.id)
            while id in step_before_ids:
                id = id + '-s'
            step_before_ids[id] = (i, j)
            for after in outcome.after:
                has_after[after.id] = True
    for i, step in enumerate(proof):
        with dot.subgraph(name='cluster_' + str(i)) as g:
            if step.tactic.which() == 'known':
                tactic_text = step.tactic.known.text
            else:
                tactic_text = 'unknown'
            g.attr(label=tactic_text)
            for j, outcome in enumerate(step.outcomes):
                if outcome.before.id not in has_after:
                    dot.node(str(outcome.before.id), label='⬤', shape='doublecircle',
                           URL = proof_outcome_url(dependencies[0], definition, i, j))

                for after in outcome.after:
                    if outcome.before.id == after.id:
                        before_id = surrogates.get(str(outcome.before.id), str(outcome.before.id))
                        after_id = before_id + '-s'
                        (stepi, outcomei) = step_before_ids[after_id]
                        g.node(after_id, label='⬤', shape='circle',
                               URL = proof_outcome_url(dependencies[0], definition, stepi, outcomei))
                        dot.edge(before_id, after_id, style='dashed')
                        surrogates[str(after.id)] = after_id
                    else:
                        before_id = surrogates.get(str(outcome.before.id), str(outcome.before.id))
                        after_id = surrogates.get(str(after.id), str(after.id))
                        (stepi, outcomei) = step_before_ids[after_id]
                        g.node(after_id, label='⬤', shape='doublecircle',
                               URL = proof_outcome_url(dependencies[0], definition, stepi, outcomei))
                        dot.edge(before_id, after_id)
                if not outcome.after:
                    qedid = str('qed-'+str(i)+'-'+str(j))
                    before_id = surrogates.get(str(outcome.before.id), str(outcome.before.id))
                    g.node(qedid, label='', shape='point')
                    dot.edge(str(before_id), str(qedid))

    file = proof_url(dependencies[0], definition)
    dot.render(os.path.splitext(file)[0], view=False, cleanup=True)

def visualize_outcome(root, graphs, definition, proof, stepi, outcomei, dependencies,
                      depth = 0, maxNodes=100, showLabel=False):
    dot = graphviz.Digraph(format='svg')
    dot.attr('graph', ordering="out")

    outcome = proof[stepi].outcomes[outcomei]
    seen = set()

    with dot.subgraph(name='cluster_before') as g:
        g.attr('graph', label="Before state")
        visualize_term(g, root, graphs, outcome.before.root, dependencies,
                       depth=depth, prefix='before', maxNodes=maxNodes, showLabel=showLabel,
                       seen=seen)

    for ai, after in enumerate(outcome.after):
        with dot.subgraph(name='cluster_after' + str(ai)) as g:
            g.attr('graph', label="After state " + str(ai))
            visualize_term(g, root, graphs, after.root, dependencies,
                           depth=depth, prefix='after'+str(ai), maxNodes=maxNodes, showLabel=showLabel,
                           seen=seen)

    with dot.subgraph(name='cluster_term') as g:
        g.attr('graph', label="Proof term")
        prefix = 'term'
        visualize_term(g, root, graphs, outcome.term, dependencies,
                       depth=depth, prefix=prefix, maxNodes=maxNodes, showLabel=False,
                       seen=seen)
        # Sometimes the subgraph is completely empty because the term is contained in another subgraph.
        # Therefore, we artificially add a extra root node
        g.node('artificial-root', 'artificial-root')
        id = get_id(0, outcome.term)
        value = graphs[0].nodes[outcome.term]
        if value.label.which() == 'definition' and prefix:
            id = prefix + '-' + id
        g.edge('artificial-root', id)


    label = graphs[0].nodes[definition].label.definition.name
    dot.attr('graph', label="Proof step " + str(stepi) + " outcome " + str(outcomei) +
             " of " + label + " from " + str(dependencies[0]))
    dot.attr('graph', fontsize="40pt")
    dot.attr('graph', labelloc="t")
    dot.attr('graph', URL=proof_url(dependencies[0], definition))

    file = proof_outcome_url(dependencies[0], definition, stepi, outcomei)
    dot.render(os.path.splitext(file)[0], view=False, cleanup=False)

def visualize_file_deps(root, deps):
    trans_deps = {}
    sparse_deps = {}

    def calc_trans_deps(f):
        if f in trans_deps:
            return
        for d in deps[f]:
            calc_trans_deps(d)
        agg = [trans_deps[d] for d in deps[f]]
        trans_deps[f] = set().union(*agg).union(set(deps[f]))
    for f in deps:
        calc_trans_deps(f)
    def calc_sparse_deps(f):
        res = []
        for d in deps[f]:
            found = False
            for d2 in deps[f]:
                if d in trans_deps[d2]:
                    found = True
            if not found:
                res.append(d)
        sparse_deps[f] = res
    for f in deps:
        calc_sparse_deps(f)

    dot = graphviz.Digraph(engine='dot', format='svg')
    dot.attr('graph', ordering="out")
    dot.attr('graph', nodesep="1")
    dot.attr('graph', ranksep="2")
    #dot.attr('graph', rankdir="LR")

    hierarchy = {'files': [], 'subdirs': {}}
    for f in sparse_deps:
        dirs = f.split('/')[:-1]
        leaf = hierarchy
        for d in dirs:
            leaf['subdirs'].setdefault(d, {'files': [], 'subdirs': {}})
            leaf = leaf['subdirs'][d]
        leaf['files'].append(f)
    def show_hierarchy(g, h, path, depth):
        for f in h['files']:
            g.node(f, f, URL = global_context_url(f))
        for d in h['subdirs']:
            g2 = graphviz.Digraph(name='cluster_' + path + '_' + d)
            show_hierarchy(g2, h['subdirs'][d], path + '_' + d, depth + 1)
            g.subgraph(g2)
    def flat_hierarchy(g, deps):
        for f in deps:
            g.node(f, f, URL = global_context_url(f))
    #show_hierarchy(dot, hierarchy, 'root', 0)
    flat_hierarchy(dot, deps)

    for f, deps in sparse_deps.items():
        for d in deps:
            dot.edge(f, d)

    file = root_file_url(root)
    dot.render(os.path.splitext(file)[0], view=False, cleanup=True)
    return file

def visualize_exception(reason):
    dot = graphviz.Digraph()
    dot.node(str(reason), str(reason))
    dot.render('python_graph', view=False)
