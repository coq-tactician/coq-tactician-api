import argparse
from multiprocessing import Pool
import os
from pathlib import Path
from functools import partial
from collections import Counter
import math
from pytact.data_reader import lowlevel_data_reader, lowlevel_to_highlevel, Definition, Node
import capnp
capnp.remove_import_hook()
import pytact.common
graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

def open_dataset(dataset_path: Path):
    global ctx
    ctx = lowlevel_data_reader(dataset_path)
    global data
    data = ctx.__enter__()
    global node_counts
    node_counts = [len(data[i].graph.nodes) for i in range(0, len(data.graph_files))]
    global hdata
    hdata = lowlevel_to_highlevel(data)

def definition_direct_dependencies(d: Definition) -> set[Definition]:
    deps: set[Definition] = set()
    seen = set()
    def recurse(node: Node):
        if node in seen:
            return
        seen.add(node)
        if d := node.definition:
            deps.add(d)
            return
        for _, child in node.children:
            recurse(child)

    seen.add(d.node)
    for _, child in d.node.children:
        recurse(child)
    return deps

def process1(args, fname: Path):
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
    graphid = data.graphs_by_filename[fname]
    freader = data[graphid]
    graph = freader.graph
    nodes_count = node_counts[graphid]
    edges_count = len(graph.edges)
    dependency_count = len(freader.dependencies)
    print(f"Checking file {fname}: {nodes_count} nodes and {edges_count} edges")

    # Check correctness of Graph struct
    for n in graph.nodes:
        if not (n.childrenIndex + n.childrenCount <= edges_count):
            raise Exception(f"{fname}: Children of node {n} are not valid indices of Graph.edges")
        n = n.label
        if (n.which() == graph_api_capnp.Graph.Node.Label.definition):
            file_definitions += 1

    for t in graph.edges:
        if t.target.depIndex >= dependency_count:
            raise Exception(f"{fname}: target.depIndex {t.target.depIndex} "
                  f"but len(g.dependencies) is {dependency_count} "
                  f"and g.dependencies = {freader.dependencies}")
        if t.target.nodeIndex >= node_counts[data.local_to_global(graphid, t.target.depIndex)]:
            raise Exception(f"{fname}: Reference for target {t} is out of bounds")

    # Check correctness of Dataset struct
    if file_definitions != len(freader.definitions):
        raise Exception(f"{fname}: Counted {file_definitions} definitions in the file, "
                        f"but Dataset.definitions has size {len(freader.definitions)}")

    hreader = hdata[fname]
    for d in hreader.definitions:
        if not d.node.definition:
            raise Exception(f"{fname}: Node {d.node} should be a definition but is not")

        # Check correctness of Definition struct
        match d.status:
            case Definition.Original():
                file_original_definitions += 1
            case Definition.Discharged(original):
                if not original.node.definition:
                    raise Exception(f"{fname}: Discharged node of definition {d.name} "
                                    f"is not a definition")
            case Definition.Substituted(original):
                if not original.node.definition:
                    raise Exception(f"{fname}: Substituted node of definition {d.name} "
                                    f"is not a definition")

        direct_deps = definition_direct_dependencies(d)
        for gc_elem in d.global_context:
            direct_deps.discard(gc_elem)
            if not direct_deps:
                print("breaking")
                break
        if direct_deps:
            raise Exception(f"{fname}: Definition {d.name} has dependencies {[d.name for d in direct_deps]}"
                            f"which are not part of its global context")

        crepr = d.cluster_representative
        if d not in d.cluster:
            raise Exception(f"{fname}: Cluster represented by {crepr.name}"
                            f"does not contain {d.name}")
        for cd in d.cluster:
            if crepr != cd.cluster_representative:
                raise Exception(f"{fname}: Cluster represented by {crepr.name} contains unrelated "
                                f"definition {cd.name}")
        if prev := d.previous:
            if not prev.node.definition:
                raise Exception(f"{fname}: Previous node of definition {d.name} is not a definition")
        for ep in d.external_previous:
            if not ep.node.definition:
                raise Exception(f"{fname}: External dependency of definition {d.name} is "
                                f"not a definition")
        if ps := d.proof:
            proofs += 1
            faithful = True
            before_states = set()
            for p in ps:
                for outcome in p.outcomes:
                    before_states.add(outcome.before.id)
            for p in ps:
                proof_steps += 1
                if t := p.tactic:
                    ident = t.ident
                    file_base_tactics_text.add(t.base_text)
                    file_base_tactics_intermtext.add(t.interm_text)
                else:
                    ident = 0
                file_tactics[ident] += 1
                if isinstance(d.status, Definition.Original):
                    file_original_tactics[ident] += 1
                    original_proof_steps += 1
                    if (t := p.tactic) and t.text == t.interm_text:
                            original_proof_steps_faithful += 1
                    else:
                        faithful = False
                for outcome in p.outcomes:
                    for after in outcome.after:
                        if after.id not in before_states:
                            raise Exception(
                                f"{fname}: After state {after} with tactic {p.tactic} "
                                f"of definition {d.name} does not have a corresponding "
                                f"before state")
                    if not isinstance(outcome.term, Node):
                        raise Exception(f"{fname}: Term of outcome {outcome} of definition "
                                        f"{d.name} is out of bounds")
                    for state in [outcome.before] + list(outcome.after):
                        root_label = state.root.label
                        if root_label.which != graph_api_capnp.Graph.Node.Label.proofState:
                            raise Exception(f"{fname}: root is proof state {state} is {root_label}")
                        if len(state._reader.context) != len(state._reader.contextNames):
                            raise Exception(f"{fname}: Length of context is different from length of"
                                            f"contextNames in proof state {state}")
                        root_children = [child for _, child in state.root.children]
                        for _, c in state.context:
                            if c not in root_children:
                                raise Exception(
                                    f"{fname}: hyp {c} of state {state} in def {d.name} is not "
                                    f"reachable from the root")
                            if c.label.which not in [graph_api_capnp.Graph.Node.Label.contextDef,
                                                     graph_api_capnp.Graph.Node.Label.contextAssum]:
                                raise Exception(
                                    f"{fname}: ctx {state.context} of a state {state} "
                                    f"has the wrong node classification {c.label}")

                    file_tactic_arguments.setdefault(ident, len(outcome.tactic_arguments))
                    if file_tactic_arguments[ident] != len(outcome.tactic_arguments):
                        raise Exception(f"{fname}: Tactic with two different argument lengths detected")
                    outcomes += 1
                    if isinstance(d.status, Definition.Original):
                        original_outcomes += 1
                    for a in outcome.tactic_arguments:
                        if not a:
                            unresolvable += 1
                        else:
                            if not isinstance(a, Node):
                                raise Exception(f"{fname}: Argument of outcome {outcome} of definition "
                                                f"{d.name} is out of bounds")
            if isinstance(d.status, Definition.Original):
                original_proofs += 1
                if faithful:
                    original_proofs_faithful += 1
    if freader.representative != len(graph.nodes):
        if (graph.nodes[freader.representative].label.which() !=
            graph_api_capnp.Graph.Node.Label.definition):
            raise Exception(f"{fname}: Representative {freader.representative} is not a definition")
    for dep in freader.dependencies:
        if Path(dep) not in data.graphs_by_filename:
            raise Exception(f"{fname}: Dependency {dep} could not be found")


    return (fname, nodes_count, edges_count,
            file_definitions, file_original_definitions, file_base_tactics_text,
            file_base_tactics_intermtext, file_tactics, file_original_tactics, file_tactic_arguments,
            proof_steps, original_proof_steps, original_proof_steps_faithful,
            outcomes, original_outcomes, proofs, original_proofs,
            original_proofs_faithful, unresolvable)

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
                        default=None,
                        help='number of parallel multiprocessing jobs to run')
    parser.add_argument('--verbose',
                       type = int,
                       default = 0,
                       help='level of being verbose 0,1..')

    args = parser.parse_args()

    import sys
    sys.setrecursionlimit(10000)

    dataset_path = Path(args.dir).resolve()

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

    with lowlevel_data_reader(dataset_path) as data:
        file_list = data.graph_files

    process1_partial = partial(process1, args)
    with Pool(args.jobs, open_dataset, [dataset_path]) as pool:
        results = pool.map(process1_partial, file_list, chunksize=1)

    for res in results:
        (fname, nodes_count, edges_count,
         file_definitions, file_original_definitions, file_base_tactics_text,
         file_base_tactics_intermtext, file_tactics, file_original_tactics, file_tactic_arguments,
         proof_steps, original_proof_steps, original_proof_steps_faithful,
         outcomes, original_outcomes, proofs, original_proofs,
         original_proofs_faithful, unresolvable) = res
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

    if (args.verbose >= 1):
        print("Tactics base text:")
        for t in base_tactics_text:
            print(t)
        print("Tactics intermtext text:")
        for t in base_tactics_intermtext:
            print(t)

if __name__ == '__main__':
    main()
