import sys
import argparse
from multiprocessing import Pool
import os
from pathlib import Path
from functools import partial
import capnp
capnp.remove_import_hook()
import pytact.common
graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

len_nodes = {}

def check_dep(fname, rootdir, _dep):
    abs_dep = os.path.join(rootdir, _dep)
    if abs_dep[-2:] == '.v':
        abs_dep = abs_dep[:-2] + '.bin'    # remove .v -> .bin correction when datasets with .v are no longer used
    if os.path.isfile(abs_dep):
        print(f"{fname}: Dependency file {abs_dep} present")
    else:
        print(f"{fname}: Error: Dependency file does not exist: {abs_dep}")
        raise Exception

def process1(rootdir, args, fname):
    with open(fname) as f:
        file_tactical_definitions = []
        file_base_tactics_text = set()
        file_base_tactics_intermtext = set()
        file_tactics = set()
        proof_steps = 0
        g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
        dep0 = g.dependencies[0]
        nodes_count = len(g.graph.classifications)
        edges_count = len(g.graph.edges)
        print(f"{fname}: nodes {nodes_count}, edges {edges_count}, dep[0] {dep0}")
        for n in g.tacticalDefinitions:
            if g.graph.classifications[n].which() != 'definition':
                print(f'{fname}: TacticalDefinitions Problem A')
                raise Exception
            if g.graph.classifications[n].definition.which() != 'tacticalConstant':
                print(f'{fname}: TacticalDefinitions Problem B with '
                      f'{g.graph.classifications[n].definition.which()}')
                raise Exception

        for n in g.graph.classifications:
            if (n.which() == 'definition'):
                if (n.definition.which() == 'tacticalConstant'):
                    file_tactical_definitions.append(n.definition.name)
                    tc = n.definition.tacticalConstant
                    for p in tc.tacticalProof:
                        proof_steps += 1
                        file_tactics.add(p.tactic.ident)
                        file_base_tactics_text.add(p.tactic.baseText)
                        file_base_tactics_intermtext.add(p.tactic.intermText)
        for _dep in g.dependencies:
            check_dep(fname, rootdir, _dep)


    return (fname, dep0, nodes_count, edges_count,
            file_tactical_definitions, file_base_tactics_text,
            file_base_tactics_intermtext, file_tactics, proof_steps)

def process2(rootdir, args, res):
    fname, _, nodes_count, _, _,  _, _, _, _  = res
    with open(fname) as f:
        print(fname)
        g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
        local_count = nodes_count
        local_max = 0
        for x in g.graph.edges:
            local_max = max(local_max, x.source)
            if x.target.depIndex == 0:
                local_max = max(local_max, x.target.nodeIndex)
            if not (x.source < local_count):
                print(f"Error: in {fname} edge {x} has source {x.source} outside of local node count {local_count}")
                raise Exception
            if not (x.target.depIndex < len(g.dependencies)):
                print(f"Error: in {fname} x.target.depIndex {x.target.depIndex} "
                      f"but len(g.dependencies) is {len(g.dependencies)} "
                      f"and g.dependencies = {g.dependencies}")
                raise Exception
            _dep = g.dependencies[x.target.depIndex]
            if _dep in len_nodes.keys():
                dep_len_nodes = len_nodes[g.dependencies[x.target.depIndex]]
            else:
                print(f"WARNING: {fname} reference to {g.dependencies[x.target.depIndex]} "
                      "is not in the index of bin files in a given dataset, "
                      "following the reference outside ")
                check_dep(fname, rootdir, _dep)
                with open(_dep,'rb') as dep_f:
                    dep_b = dep_f.read()
                dep_g = graph_api_capnp.Dataset.from_bytes_packed(dep_b, traversal_limit_in_words=2**64-1)
                dep_len_nodes = len(dep_g.graph.classifications)
            if not x.target.nodeIndex < dep_len_nodes:
                print(f"in {fname} reference to {g.dependencies[x.target.depIndex]} "
                      f"with node {x.target.nodeIndex} but len_nodes[g.dependencies[x.target.depIndex]] "
                      f"is {len_nodes[g.dependencies[x.target.depIndex]]}")
                raise Exception
        if local_max != local_count - 1:
            print(f"{fname}: WARNING: max local source/local target of edges is not equal to max node")
        for node_index in g.tacticalDefinitions:
            node_classification = g.graph.classifications[node_index]
            if node_classification.which() != 'definition':
                print(f"{fname}: ERROR: the node type of tacticialDefinition is not definition but "
                      f"{node_classification.which()}")
                raise Exception
            else:
                proof_steps = node_classification.definition.tacticalConstant.tacticalProof
                for x in proof_steps:
                    if not (x.state.root < local_count):
                        print(f"{fname}: root {x.state.root} of a state {x.state} is "
                              f"outside local node count {local_count}")
                        raise Exception


def main():
    parser = argparse.ArgumentParser(
        description = 'sanity of check of *.bin dataset with labelled_graph_api',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('dir',
                        type=str,
                        help='the directory of the dataset')
    parser.add_argument('--jobs',
                        type=int,
                        default=4,
                        help='number of parallel multiprocessing jobs to run')
    parser.add_argument('--verbose',
                       type = int,
                       default = 0,
                       help='level of being verbose 0,1..')

    args = parser.parse_args()
    rootdir = Path(os.path.expanduser(args.dir))

    tactics = set()
    base_tactics_text = set()
    base_tactics_intermtext = set()
    tactical_definitions = []
    nodes_total = 0
    edges_total = 0
    proof_steps_total = 0

    file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

    process1_partial = partial(process1, rootdir, args)
    with Pool(args.jobs) as pool:
        results = pool.map(process1_partial, file_list, chunksize=20)

    for res in results:
        (fname, dep0, nodes_count, edges_count,
         file_tactical_definitions, file_base_tactics_text,
         file_base_tactics_intermtext,
         file_tactics, proof_steps) = res
        len_nodes[dep0] = nodes_count
        nodes_total += nodes_count
        edges_total += edges_count
        proof_steps_total += proof_steps
        tactical_definitions.extend(file_tactical_definitions)
        base_tactics_text.update(file_base_tactics_text)
        base_tactics_intermtext.update(file_base_tactics_intermtext)
        tactics.update(file_tactics)

    print(f"Nodes total {nodes_total}")
    print(f"Edges total {edges_total}")
    print(f"Tactics total {len(tactics)}")
    print(f"Tactics base text total {len(base_tactics_text)}")
    print(f"Tactics base intermtext total {len(base_tactics_intermtext)}")
    print(f"Tactical definitions total {len(tactical_definitions)}")
    print(f"Proof steps total {proof_steps_total}")

    if (args.verbose >=1):
        print("Tactics base text:")
        for t in base_tactics_text:
            print(t)
        print("Tactics intermtext text:")
        for t in base_tactics_intermtext:
            print(t)

    process2_partial = partial(process2, rootdir, args)
    with Pool(args.jobs) as pool:
        pool.map(process2_partial, results)


if __name__ == '__main__':
    main()
