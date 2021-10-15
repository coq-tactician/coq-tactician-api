#!/usr/bin/env python3
import graphviz

def visualize(graph, state):
    nodes = graph.classifications
    edges = graph.edges
    root = state.root
    context = state.context
    assert all(n < len(nodes) for n in context)

    dot = graphviz.Digraph()
    for node, label in enumerate(nodes):
        label = label.which()
        if node in context: label = str(node) + ': ['+label+']'
        if node == root:
            label = "Root: "+label
        dot.node(str(node), label)

    dot.edges([
        (str(edge.source), str(edge.target.nodeIndex))
        for edge in edges
    ])
    dot.render('python_graph', view=False)

def visualize_exception(reason):
    dot = graphviz.Digraph()
    dot.node(str(reason), str(reason))
    dot.render('python_graph', view=False)
