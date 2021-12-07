import sys
import capnp
import dag_api_capnp

print (dag_api_capnp.Dag.nodeArities)

print(sys.argv[1])
f = open(sys.argv[1])
g = dag_api_capnp.Dag.read_packed(f, traversal_limit_in_words=2**64-1)
print(g)
