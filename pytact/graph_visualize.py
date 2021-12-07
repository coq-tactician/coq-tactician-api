import os
import sys

import graphviz

# Load the cap'n proto library, and the communication specification in 'graph_api.capnp'
import capnp
capnp.remove_import_hook()
graph_api_capnp = os.path.join(sys.prefix,'share','pytact','labelled_graph_api.capnp')
graph_api_capnp = capnp.load(graph_api_capnp)

arrow_heads = [ "dot", "inv", "odot", "invdot", "invodot" ]
edge_arrow_map = {}
for group in graph_api_capnp.groupedEdges:
    count = 0
    for sort in group.conflatable:
        edge_arrow_map[sort] = arrow_heads[count]
        count += 1

def visualize(graph, state, showLabel = False):
    nodes = graph.classifications
    edges = graph.edges
    root = state.root
    context = state.context
    assert all(n < len(nodes) for n in context)

    dot = graphviz.Digraph()
    dot.attr('graph', ordering="out")
    for node, label in enumerate(nodes):
        label = label.which()
        if node in context: label = str(node) + ': ['+label+']'
        if node == root:
            label = "Root: "+label
        dot.node(str(node), label)

    for edge in edges:
        if showLabel:
            label = str(edge.sort)
        else:
            label = ""
        if str(edge.sort) in ["appArgOrder", "evarSubstOrder"]:
            constraint = "false"
        else:
            constraint = "true"
        dot.edge(str(edge.source), str(edge.target.nodeIndex), label=label,
                 arrowtail=edge_arrow_map[edge.sort], dir="both", constraint=constraint)

    dot.render('python_graph', view=False)

def visualize_exception(reason):
    dot = graphviz.Digraph()
    dot.node(str(reason), str(reason))
    dot.render('python_graph', view=False)
