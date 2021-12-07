import sys
import capnp
import graph_api_capnp

print(sys.argv[1])
f = open(sys.argv[1])
g = graph_api_capnp.Graph.read_packed(f, traversal_limit_in_words=2**64-1)
print(g)
