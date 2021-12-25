import sys
import os
from pathlib import Path
import pytact.common

import capnp
capnp.remove_import_hook()
graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

tactics = set()
base_tactics_text = set()
tactical_definitions = []
max_node = {}
node_total = 0
edges_total = 0
proof_steps_total = 0

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
    for n in g.tacticalDefinitions:
        if g.graph.classifications[n].which() != 'definition':
            print('TacticalDefinitions Problem A')
        if g.graph.classifications[n].definition.which() != 'tacticalConstant':
            print('TacticalDefinitions Problem B')
    for n in g.graph.classifications:
        if (n.which() == 'definition'):
            if (n.definition.which() == 'tacticalConstant'):
                tactical_definitions.append(n.definition.name)
                tc = n.definition.tacticalConstant
                for p in tc.tacticalProof:
                    proof_steps_total += 1
                    tactics.add(p.tactic.ident)
                    base_tactics_text.add(p.tactic.baseText)

print("Node total")
print(node_total)
print("Edge total")
print(edges_total)
print("Tactics total")
print(len(tactics))
print("Tactics base text total")
print(len(base_tactics_text))
print("Tactical definitions total")
print(len(tactical_definitions))
print("Proof steps total")
print(proof_steps_total)

for fname in file_list:
    print(fname)
    f = open(fname)
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
        if not (x.target.depIndex < len(g.dependencies)):
            print(f"Error: x.target.depIndex {x.target.depIndex} but len(g.dependencies) is {len(g.dependencies)}")
            print(f"file is {fname} edge is {x}")
            print(f"g.dependencies = {g.dependencies}")
            sys.exit(1)
            
        if max_node[g.dependencies[x.target.depIndex]] <= x.target.nodeIndex:
            print(x)
            print("ProblemB")
    if local_max != local_count - 1:
        print("ProblemC")
