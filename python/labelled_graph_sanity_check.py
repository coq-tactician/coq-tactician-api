import sys
import os
from pathlib import Path

import capnp
capnp.remove_import_hook()
graph_api_capnp = str(Path('labelled_graph_api.capnp').expanduser())
graph_api_capnp = capnp.load(graph_api_capnp)

tactics = set()
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
    g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
    dep = g.dependencies[0]
    node_count = len(g.graph.classifications)
    max_node[dep] = node_count
    node_total += node_count
    edges_total += len(g.graph.edges)
    print(max_node[dep])
    for ps in g.proofSteps:
        tactics.add(ps.tactic.ident)

print("Node total")
print(node_total)
print("Edge total")
print(edges_total)
print("Tactics total")
print(len(tactics))

for f in file_list:
    print(f)
    f = open(f)
    g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
    dep = g.dependencies[0]
    local_max = 0
    local_count = max_node[dep]
    for x in g.graph.edges:
        local_max = max(local_max, x.source)
        if x.target.depIndex == 0:
            local_max = max(local_max, x.target.nodeIndex)
        if local_count <= x.source:
            print(x)
            print("ProblemA")
        if max_node[g.dependencies[x.target.depIndex]] <= x.target.nodeIndex:
            print(x)
            print("ProblemB")
    if local_max != local_count - 1:
        print("ProblemC")
    for x in g.proofSteps:
        if local_count <= x.state.root:
            print("ProblemD")
        nt = g.graph.classifications[x.state.root]
        if nt.which() != 'root':
            print("ProblemE")
