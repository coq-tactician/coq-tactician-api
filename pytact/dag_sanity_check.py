import sys
import os
from pathlib import Path
import capnp
import dag_api_capnp

arities = {}
for a in dag_api_capnp.Dag.nodeArities:
    if a.arity.which() == 'fixed':
        arities[a.kind] = a.arity.fixed
    else:
        arities[a.kind] = -1

max_node = {}
node_total = 0

rootdir = sys.argv[1]
os.chdir(rootdir)
rootdir = Path(os.getcwd())
file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

for f in file_list:
    print(f)
    f = open(f)
    g = dag_api_capnp.Dag.read_packed(f, traversal_limit_in_words=2**64-1)
    dep = g.dependencies[0]
    node_count = len(g.nodes)
    max_node[dep] = node_count
    node_total += node_count
    print(max_node[dep])

print("Node total")
print(node_total)

for f in file_list:
    print(f)
    f = open(f)
    g = dag_api_capnp.Dag.read_packed(f, traversal_limit_in_words=2**64-1)
    dep = g.dependencies[0]
    local_count = max_node[dep]
    for x in g.nodes:
        if x.which() == 'node':
            children = x.node.children
            expectedArity = arities[x.node.kind]
            if expectedArity != -1 and expectedArity != len(children):
                print(x)
                print("ProblemA")
            for c in children:
                if max_node[g.dependencies[c.depIndex]] <= c.nodeIndex:
                    print(x)
                    print("ProblemB")
    for x in g.proofSteps:
        if local_count <= x.node:
            print(x)
            print(local_count)
            print(x.node)
            print("ProblemD")
        # nt = g.nodes[x.node]
        # print(nt)
