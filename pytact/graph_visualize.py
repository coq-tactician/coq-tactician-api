import os
import sys
import logging

import graphviz

# Load the cap'n proto library, and the communication specification in 'graph_api.capnp'
import capnp
capnp.remove_import_hook()

import pytact.common


labelled_graph_api = capnp.load(pytact.common.labelled_graph_api_filename)

arrow_heads = [ "dot", "inv", "odot", "invdot", "invodot" ]
edge_arrow_map = {}

logger = logging.getLogger(__name__)


for group in labelled_graph_api.groupedEdges:
    cnt = 0
    for sort in group.conflatable:
        edge_arrow_map[sort] = arrow_heads[cnt]
        cnt += 1

def visualize(graph, state, showLabel = False, filename='python_graph', cleanup=True):
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

    dot.render(filename, view=False, cleanup=cleanup)

def visualize_exception(reason, filename='python_graph', cleanup=True):
    dot = graphviz.Digraph()
    dot.node(str(reason), str(reason))
    dot.render(filename, view=False, cleanup=cleanup)


class Visualizer:
    def __init__(self, filename, count, show_labels, cleanup):
        self.filename = filename
        self.cnt = 0
        self.count = count
        self.show_labels = show_labels
        self.cleanup = cleanup
        logger.error("with self.count %d", self.count)
    def _visualize(self, filename, result):
        if result.which() == 'newState':
            visualize(result.newState.graph, result.newState.state,
                         filename=filename, showLabel=self.show_labels, cleanup=self.cleanup)
        else:
            visualize_exception(result, filename=self.filename, cleanup=self.cleanup)

    def render(self, result):
        filename = self.filename
        if self.count:
            filename += str(self.cnt)
            self.cnt += 1

        self._visualize(filename, result)

