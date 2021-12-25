import sys
import argparse
from multiprocessing import Pool
import os
from pathlib import Path
import pytact.common
import capnp
capnp.remove_import_hook()
graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

max_node = {}


def process1(fname):
    with open(fname) as f:
        print(fname)
        file_tactical_definitions = []
        file_base_tactics_text = set()
        file_tactics = set()
        proof_steps = 0
        g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
        dep = g.dependencies[0]
        nodes_count = len(g.graph.classifications)
        edges_count = len(g.graph.edges)
        print(nodes_count)
        for n in g.tacticalDefinitions:
            if g.graph.classifications[n].which() != 'definition':
                print('TacticalDefinitions Problem A')
            if g.graph.classifications[n].definition.which() != 'tacticalConstant':
                print('TacticalDefinitions Problem B')
        for n in g.graph.classifications:
            if (n.which() == 'definition'):
                if (n.definition.which() == 'tacticalConstant'):
                    file_tactical_definitions.append(n.definition.name)
                    tc = n.definition.tacticalConstant
                    for p in tc.tacticalProof:
                        proof_steps += 1
                        file_tactics.add(p.tactic.ident)
                        file_base_tactics_text.add(p.tactic.baseText)
    return (fname, dep, nodes_count, edges_count, file_tactical_definitions, file_base_tactics_text, file_tactics, proof_steps)

def process2(res):
    fname, _, nodes_count, _, _,  _, _, _  = res
    print(fname)
    with open(fname) as f:
        g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
        local_count = nodes_count
        local_max = 0
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
                raise Exception

            if max_node[g.dependencies[x.target.depIndex]] <= x.target.nodeIndex:
                print(x)
                print("ProblemB")
        if local_max != local_count - 1:
            print("ProblemC")


def main():
    parser = argparse.ArgumentParser(
        description = 'sanity of check of *.bin dataset with labelled_graph_api',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('--dir',
                        type=str,
                        default=os.getcwd(),
                        help='directory of the dataset')
    parser.add_argument('--jobs',
                        type=int,
                        default=4,
                        help='number of parallel multiprocessing jobs to run')

    args = parser.parse_args()
    rootdir = Path(os.path.expanduser(args.dir))

    tactics = set()
    base_tactics_text = set()
    tactical_definitions = []
    nodes_total = 0
    edges_total = 0
    proof_steps_total = 0

    file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

    with Pool(args.jobs) as pool:
        results = pool.map(process1, file_list, chunksize=20)

    for res in results:
        fname, dep, nodes_count, edges_count, file_tactical_definitions, file_base_tactics_text, file_tactics, proof_steps = res
        print(fname)
        max_node[dep] = nodes_count
        nodes_total += nodes_count
        edges_total += edges_count
        proof_steps_total += proof_steps
        tactical_definitions.extend(file_tactical_definitions)
        base_tactics_text.update(file_base_tactics_text)
        tactics.update(file_tactics)

    print(f"Nodes total {nodes_total}")
    print(f"Edges total {edges_total}")
    print(f"Tactics total {len(tactics)}")
    print(f"Tactics base text total {len(base_tactics_text)}")
    print(f"Tactical definitions total {len(tactical_definitions)}")
    print(f"Proof steps total {proof_steps_total}")

    with Pool(args.jobs) as pool:
        pool.map(process2, results)


if __name__ == '__main__':
    main()
