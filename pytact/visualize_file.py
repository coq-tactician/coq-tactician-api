from pathlib import Path
import sys
import os

import capnp
capnp.remove_import_hook()
import pytact.common
import pytact.graph_visualize as gv

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

def main():
    print(sys.argv[1])
    f = open(sys.argv[1])
    g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
    gv.visualize_global_context(g.graph, g.definitions, g.representative, g.dependencies)
