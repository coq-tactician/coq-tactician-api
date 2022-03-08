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

def visualize_exception(reason):
    dot = graphviz.Digraph()
    dot.node(str(reason), str(reason))
    dot.render('python_graph', view=False)
