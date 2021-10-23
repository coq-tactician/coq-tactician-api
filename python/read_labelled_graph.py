from pathlib import Path
import sys

import capnp
capnp.remove_import_hook()
graph_api_capnp = str(Path('labelled_graph_api.capnp').expanduser())
graph_api_capnp = capnp.load(graph_api_capnp)

print(sys.argv[1])
f = open(sys.argv[1])
g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
print(g)
