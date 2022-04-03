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
            if str(edge.label) in ["appArgOrder", "evarSubstOrder"]:
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
            if str(edge.label) in ["appArgOrder", "evarSubstOrder"]:
                constraint = "false"
            else:
                constraint = "true"
            dot.edge(str(node), str(edge.target.nodeIndex), label=label,
                     arrowtail=edge_arrow_map[edge.label], dir="both", constraint=constraint)

    dot.render('python_graph', view=False)

def visualize_global_context(graph, definitions, representative, dependencies, showLabel = False):
    nodes = graph.nodes

    dot = graphviz.Digraph()
    dot.attr('graph', ordering="out")
    for node in definitions:
        value = nodes[node]
        if node == representative:
            label = 'Representative: ' + value.label.definition.name
        else:
            label = value.label.definition.name
        dot.node(str(node), label)
        dot.edge(str(node), str(value.label.definition.previous),
                 arrowtail="dot", dir="both", constraint="true")
        for fi in value.label.definition.externalPrevious:
            file_label = 'File:' + dependencies[fi]
            fn = 'file-'+str(fi)
            dot.node(fn, file_label)
            dot.edge(str(node), fn,
                     arrowtail="dot", dir="both", constraint="true")
        if value.label.definition.status.which () == 'discharged':
            dot.edge(str(node), str(value.label.definition.status.discharged),
                     arrowtail="inv", dir="both", constraint="false", style="dashed")
        if value.label.definition.status.which () == 'substituted':
            if value.label.definition.status.substituted.depIndex == 0:
                dot.edge(str(node), str(value.label.definition.status.substituted.nodeIndex),
                         arrowtail="odot", dir="both", constraint="false", style="dashed")
            else:
                dot.edge(str(node), 'External',
                         arrowtail="odot", dir="both", constraint="false", style="dashed")

    dot.render('python_graph', view=False)

def visualize_exception(reason):
    dot = graphviz.Digraph()
    dot.node(str(reason), str(reason))
    dot.render('python_graph', view=False)
