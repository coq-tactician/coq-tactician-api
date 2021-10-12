import sys
import os
from pathlib import Path
import capnp
import graph_api_capnp

max_node = {}
node_total = 0
edges_total = 0

rootdir = sys.argv[1]
os.chdir(rootdir)
rootdir = Path(os.getcwd())
file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

for f in file_list:
    print(f)
    f = open(f)
    g = graph_api_capnp.Graph.read_packed(f, traversal_limit_in_words=2**64-1)
    dep = g.dependencies[0]
    node_count = len(g.classifications)
    max_node[dep] = node_count
    node_total += node_count
    edges_total += len(g.edges)
    print(max_node[dep])

print("Node total")
print(node_total)
print("Edge total")
print(edges_total)

for f in file_list:
    print(f)
    f = open(f)
    g = graph_api_capnp.Graph.read_packed(f, traversal_limit_in_words=2**64-1)
    dep = g.dependencies[0]
    local_max = 0
    local_count = max_node[dep]
    for x in g.edges:
        myfrom = getattr(x, 'from')  # From collides with python reserved keywords
        local_max = max(local_max, myfrom)
        if x.toward.depIndex == 0:
            local_max = max(local_max, x.toward.nodeIndex)
        if local_count <= myfrom:
            print(x)
            print("ProblemA")
        if max_node[g.dependencies[x.toward.depIndex]] <= x.toward.nodeIndex:
            print(x)
            print("ProblemB")
    if local_max != local_count - 1:
        print("ProblemC")
    for x in g.proofSteps:
        if local_count <= x.node:
            print("ProblemD")
        nt = g.classifications[x.node]
        if nt.which() != 'root':
            print("ProblemE")
