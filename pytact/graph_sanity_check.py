import sys
import argparse
from multiprocessing import Pool
import os
from pathlib import Path
from functools import partial
from collections import Counter
import math
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
        # print(f"{fname}: Dependency file {abs_dep} present")
        pass
    else:
        print(f"{fname}: Error: Dependency file does not exist: {abs_dep}")
        raise Exception

def process1(rootdir, args, fname):
    with open(fname) as f:
        file_definitions = 0
        file_original_definitions = 0
        file_base_tactics_text = set()
        file_base_tactics_intermtext = set()
        file_tactics = Counter()
        file_original_tactics = Counter()
        file_tactic_arguments = {}
        proof_steps = 0
        original_proof_steps = 0
        original_proof_steps_faithful = 0
        outcomes = 0
        original_outcomes = 0
        proofs = 0
        original_proofs = 0
        original_proofs_faithful = 0
        unresolvable = 0
        g = graph_api_capnp.Dataset.read(f, traversal_limit_in_words=2**64-1)
        dep0 = g.dependencies[0]
        nodes_count = len(g.graph.nodes)
        edges_count = len(g.graph.edges)
        print(f"{fname}: nodes {nodes_count}, edges {edges_count}, dep[0] {dep0}")
        for n in g.definitions:
            if g.graph.nodes[n].label.which() != 'definition':
                print(f'{fname}: TacticalDefinitions Problem A')
                raise Exception

        for n in g.graph.nodes:
            if not (n.childrenIndex + n.childrenCount <= edges_count):
                print(f'{fname}: Children of node {n} are not valid indices of edges')
                raise Exception
            n = n.label
            if (n.which() == 'definition'):
                file_definitions += 1
                if n.definition.status.which() == 'original':
                    file_original_definitions += 1
                if n.definition.which() == 'tacticalConstant' or n.definition.which() == 'tacticalSectionConstant':
                    if n.definition.which() == 'tacticalConstant':
                        ps = n.definition.tacticalConstant
                    else:
                        ps = n.definition.tacticalSectionConstant
                    faithful = True
                    before_states = set()
                    for p in ps:
                        for outcome in p.outcomes:
                            before_states.add(outcome.before.id)
                    for p in ps:
                        for outcome in p.outcomes:
                            for after in outcome.after:
                                if after.id not in before_states:
                                    print(f'{fname}: After state {after} with tactic {p.tactic.text} of definition {n.definition.name} does not have a correspoinding before state')
                                    raise Exception
                    for p in ps:
                        proof_steps += 1
                        if n.definition.status.which() == 'original':
                            original_proof_steps += 1
                        if (n.definition.status.which() == 'original'
                            and p.tactic.which() == 'known'
                            and p.tactic.known.text == p.tactic.known.intermText):
                            original_proof_steps_faithful += 1
                        else:
                            faithful = False
                        if p.tactic.which() == 'known':
                            ident = p.tactic.known.ident
                        else:
                            ident = 0
                        file_tactics[ident] += 1
                        if n.definition.status.which() == 'original':
                            file_original_tactics[ident] += 1
                        for outcome in p.outcomes:
                            file_tactic_arguments.setdefault(ident, len(outcome.tacticArguments))
                            if file_tactic_arguments[ident] != len(outcome.tacticArguments):
                                print(f'{fname}: Tactic with two different argument lengths detected')
                                raise Exception
                            outcomes += 1
                            if n.definition.status.which() == 'original':
                                original_outcomes += 1
                            for a in outcome.tacticArguments:
                                if a.which() == 'unresolvable':
                                    unresolvable += 1
                        if p.tactic.which() == 'known':
                            file_base_tactics_text.add(p.tactic.known.baseText)
                            file_base_tactics_intermtext.add(p.tactic.known.intermText)
                    proofs += 1
                    if n.definition.status.which() == 'original':
                        original_proofs += 1
                    if n.definition.status.which() == 'original' and faithful:
                        original_proofs_faithful += 1
        for _dep in g.dependencies:
            check_dep(fname, rootdir, _dep)
        # Needed to work around this annoying bug: https://github.com/capnproto/pycapnp/issues/82
        g.total_size


    return (fname, dep0, nodes_count, edges_count,
            file_definitions, file_original_definitions, file_base_tactics_text,
            file_base_tactics_intermtext, file_tactics, file_original_tactics, file_tactic_arguments,
            proof_steps, original_proof_steps, original_proof_steps_faithful,
            outcomes, original_outcomes, proofs, original_proofs,
            original_proofs_faithful, unresolvable)

def process2(rootdir, args, res):
    fname, _, nodes_count, _, _,  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _  = res
    with open(fname) as f:
        print(fname)
        g = graph_api_capnp.Dataset.read(f, traversal_limit_in_words=2**64-1)
        local_count = nodes_count
        for t in g.graph.edges:
            if not (t.target.depIndex < len(g.dependencies)):
                print(f"Error: in {fname} x.target.depIndex {x.target.depIndex} "
                      f"but len(g.dependencies) is {len(g.dependencies)} "
                      f"and g.dependencies = {g.dependencies}")
                raise Exception
            _dep = g.dependencies[t.target.depIndex]
            if _dep in len_nodes.keys():
                dep_len_nodes = len_nodes[g.dependencies[t.target.depIndex]]
            else:
                print(f"WARNING: {fname} reference to {g.dependencies[x.target.depIndex]} "
                      "is not in the index of bin files in a given dataset, "
                      "following the reference outside ")
                check_dep(fname, rootdir, _dep)
                with open(_dep,'rb') as dep_f:
                    dep_b = dep_f.read()
                    dep_g = graph_api_capnp.Dataset.from_bytes_packed(dep_b, traversal_limit_in_words=2**64-1)
                    dep_len_nodes = len(dep_g.graph.classifications)
                    # Needed to work around this annoying bug: https://github.com/capnproto/pycapnp/issues/82
                    g.total_size
            if not t.target.nodeIndex < dep_len_nodes:
                print(f"in {fname} reference to {g.dependencies[x.target.depIndex]} "
                      f"with node {x.target.nodeIndex} but len_nodes[g.dependencies[x.target.depIndex]] "
                      f"is {len_nodes[g.dependencies[x.target.depIndex]]}")
                raise Exception
        for node_index in g.definitions:
            node_classification = g.graph.nodes[node_index].label
            if node_classification.definition.which() == 'tacticalConstant':
                proof_steps = node_classification.definition.tacticalConstant
            elif node_classification.definition.which() == 'tacticalSectionConstant':
                proof_steps = node_classification.definition.tacticalSectionConstant
            else:
                proof_steps = []
            for x in proof_steps:
                for outcome in x.outcomes:
                    if not (outcome.before.root < local_count):
                        print(f"{fname}: root {outcome.before.root} of a state {outcome.before} is "
                              f"outside local node count {local_count}")
                        raise Exception
                    for c in outcome.before.context:
                        if not (c < local_count):
                            print(f"{fname}: ctx {outcome.before.context} of a state {outcome.before} is "
                                  f"outside local node count {local_count}")
                            raise Exception
                        cl = g.graph.nodes[c].label.which()
                        if not (cl == 'contextDef' or cl == 'contextAssum'):
                            print(f"{fname}: ctx {outcome.before.context} of a state {outcme.before} "
                                  f"has the wrong node classification {cn.label}")
                            raise Exception
                    for a in outcome.tacticArguments:
                        if a.which () == 'unresolvable':
                            pass
                        elif a.which() == 'term':
                            if not (a.term.nodeIndex < len_nodes[g.dependencies[a.term.depIndex]]):
                                print(f"{fname} argument {a} of proof step {x} is not resolvable")
                                raise Exception
                            # TODO: We should check that this node is actually a definition
                        else:
                            print(f"{fname}: unknown tactical argument {a}")
        # Needed to work around this annoying bug: https://github.com/capnproto/pycapnp/issues/82
        g.total_size

def entropy(d):
    n = sum(d.values())
    return -sum([(c / n) * math.log(c / n, 2) for c in d.values()])

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

    tactics = Counter()
    original_tactics = Counter()
    tactic_arguments = {}
    base_tactics_text = set()
    base_tactics_intermtext = set()
    definitions = 0
    original_definitions = 0
    nodes_total = 0
    edges_total = 0
    proof_steps_total = 0
    original_proof_steps_total = 0
    original_proof_steps_faithful_total = 0
    outcomes_total = 0
    original_outcomes_total = 0
    proofs_total = 0
    original_proofs_total = 0
    original_proofs_faithful_total = 0
    unresolvable_total = 0

    file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

    process1_partial = partial(process1, rootdir, args)
    with Pool(args.jobs) as pool:
        results = pool.map(process1_partial, file_list, chunksize=20)

    for res in results:
        (fname, dep0, nodes_count, edges_count,
         file_definitions, file_original_definitions, file_base_tactics_text,
         file_base_tactics_intermtext, file_tactics, file_original_tactics, file_tactic_arguments,
         proof_steps, original_proof_steps, original_proof_steps_faithful,
         outcomes, original_outcomes, proofs, original_proofs,
         original_proofs_faithful, unresolvable) = res
        len_nodes[dep0] = nodes_count
        nodes_total += nodes_count
        edges_total += edges_count
        proof_steps_total += proof_steps
        original_proof_steps_total += original_proof_steps
        original_proof_steps_faithful_total += original_proof_steps_faithful
        outcomes_total += outcomes
        original_outcomes_total += original_outcomes
        proofs_total += proofs
        original_proofs_total += original_proofs
        original_proofs_faithful_total += original_proofs_faithful
        definitions +=  file_definitions
        original_definitions += file_original_definitions
        base_tactics_text.update(file_base_tactics_text)
        base_tactics_intermtext.update(file_base_tactics_intermtext)
        tactics += file_tactics
        original_tactics += file_original_tactics
        for tac, length in file_tactic_arguments.items():
            tactic_arguments.setdefault(tac, length)
            if tactic_arguments[tac] != length:
                print(f'{fname}: Tactic with two different argument lengths detected')
                raise Exception

        unresolvable_total += unresolvable

    print(f"Nodes total {nodes_total}")
    print(f"Edges total {edges_total}")
    print(f"Tactics total {len(tactics)}")
    print(f"Original tactics total {len(original_tactics)}")
    print(f"Tactics entropy (bits) {entropy(tactics)}")
    print(f"Original tactics entropy (bits) {entropy(original_tactics)}")
    print(f"Tactics base text total {len(base_tactics_text)}")
    print(f"Tactics base intermtext total {len(base_tactics_intermtext)}")
    print(f"Definitions total {definitions}")
    print(f"Original definitions total {original_definitions}")
    print(f"Proof steps total {proof_steps_total}")
    print(f"Original proof steps total {original_proof_steps_total}")
    print(f"Faithfully represented original proof steps total {original_proof_steps_faithful_total}")
    print(f"Outcomes total {outcomes_total}")
    print(f"Original outcomes total {original_outcomes_total}")
    print(f"Proofs total {proofs_total}")
    print(f"Original proofs total {original_proofs_total}")
    print(f"Faithful original proofs total {original_proofs_faithful_total}")
    print(f"Unresolvable tactic arguments {unresolvable_total}")

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
