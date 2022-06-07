from pathlib import Path
import sys
import os

import capnp
capnp.remove_import_hook()
import pytact.common

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

print(sys.argv[1])
f = open(sys.argv[1])
g = graph_api_capnp.Dataset.read(f, traversal_limit_in_words=2**64-1)
print(g)
