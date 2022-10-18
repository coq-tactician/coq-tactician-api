import argparse
from multiprocessing import Pool
import os
from pathlib import Path
from functools import partial
from collections import Counter
import math
from pytact.data_reader import lowlevel_data_reader
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

def get_cluster(d):
    match d.which:
        case "inductive":
            return d.inductive
        case "constructor":
            return d.constructor
        case "projection":
            return d.projection
        case _:
            return None

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
    for n in freader.definitions:
        if graph.nodes[n].label.which != graph_api_capnp.Graph.Node.Label.definition:
            raise Exception(f"{fname}: Node {n} should be a definition but is not")

        # Check correctness of Definition struct
        definition = graph.nodes[n].label.definition
        status = definition.status
        match status.which:
            case graph_api_capnp.Definition.Status.original:
                file_original_definitions += 1
            case graph_api_capnp.Definition.Status.discharged:
                if (graph.nodes[status.discharged].label.which !=
                    graph_api_capnp.Graph.Node.Label.definition):
                    raise Exception(f"{fname}: Discharged node of definition {definition.name} "
                                    f"is not a definition")
            case graph_api_capnp.Definition.Status.substituted:
                if (data[data.local_to_global(graphid, status.substituted.depIndex)]
                    .graph.nodes[status.substituted.nodeIndex].label.which !=
                    graph_api_capnp.Graph.Node.Label.definition):
                    raise Exception(f"{fname}: Substituted node of definition {definition.name} "
                                    f"is not a definition")
        cluster = get_cluster(definition)
        if cluster != None:
            parent = cluster
            while parent != n:
                if parent >= nodes_count:
                    raise Exception(f"{fname}: Cluster {cluster} {graph.nodes[cluster].label.definition.name} "
                                    f"does not contain {n} {definition.name}")
                pd = graph.nodes[parent].label.definition
                pc = get_cluster(pd)
                if pc == None or pc != cluster:
                    raise Exception(f"{fname}: Cluster of {n} {definition.name} contains unrelated "
                                    f"definition {parent} {pd.name}")
                parent = pd.previous
        if (definition.previous != nodes_count and
            graph.nodes[definition.previous].label.which != graph_api_capnp.Graph.Node.Label.definition):
            raise Exception(f"{fname}: Previous node of definition {definition.name} is not a definition")
        for ep in definition.externalPrevious:
            if ep >= dependency_count:
                raise Exception(f"{fname}: External dependency of definition {definition.name} is "
                                f"out of bounds")
        if definition.which() == 'tacticalConstant' or definition.which() == 'tacticalSectionConstant':
            if definition.which() == 'tacticalConstant':
                ps = definition.tacticalConstant
            else:
                ps = definition.tacticalSectionConstant
            faithful = True
            before_states = set()
            for p in ps:
                for outcome in p.outcomes:
                    before_states.add(outcome.before.id)
            for p in ps:
                proof_steps += 1
                if definition.status.which == graph_api_capnp.Definition.Status.original:
                    original_proof_steps += 1
                    if (p.tactic.which() == graph_api_capnp.ProofStep.Tactic.known
                        and p.tactic.known.text == p.tactic.known.intermText):
                        original_proof_steps_faithful += 1
                    else:
                        faithful = False
                if p.tactic.which() == graph_api_capnp.ProofStep.Tactic.known:
                    ident = p.tactic.known.ident
                    file_base_tactics_text.add(p.tactic.known.baseText)
                    file_base_tactics_intermtext.add(p.tactic.known.intermText)
                else:
                    ident = 0
                file_tactics[ident] += 1
                if definition.status.which() == graph_api_capnp.Definition.Status.original:
                    file_original_tactics[ident] += 1
                for outcome in p.outcomes:
                    for after in outcome.after:
                        if after.id not in before_states:
                            raise Exception(
                                f"{fname}: After state {after} with tactic {p.tactic.text} "
                                f"of definition {definition.name} does not have a corresponding "
                                f"before state")
                    if (outcome.term.nodeIndex >=
                        node_counts[data.local_to_global(graphid, outcome.term.depIndex)]):
                        raise Exception(f"{fname}: Term of outcome {outcome} of definition "
                                        f"{definition.name} is out of bounds")
                    for state in [outcome.before] + list(outcome.after):
                        root_label = (data[data.local_to_global(graphid, state.root.depIndex)].graph
                                      .nodes[state.root.nodeIndex].label.which)
                        if root_label != graph_api_capnp.Graph.Node.Label.proofState:
                            raise Exception(f"{fname}: root is proof state {state} is {root_label}")
                        if len(state.context) != len(state.contextNames):
                            raise Exception(f"{fname}: Length of context is different from length of"
                                            f"contextNames in proof state {state}")
                        root_graphid = data.local_to_global(graphid, state.root.depIndex)
                        root_graph = data[root_graphid].graph
                        root_node = root_graph.nodes[state.root.nodeIndex]
                        root_children = [(data.local_to_global(root_graphid,
                                                               root_graph.edges[i].target.depIndex),
                                         root_graph.edges[i].target.nodeIndex) for i in
                                         range(root_node.childrenIndex,
                                               root_node.childrenIndex+root_node.childrenCount)]
                        for c in state.context:
                            if (data.local_to_global(graphid, c.depIndex), c.nodeIndex) not in root_children:
                                raise Exception(
                                    f"{fname}: hyp {c} of state {state} in def {definition.name} is not "
                                    f"reachable from the root")
                            c_label = (data[data.local_to_global(graphid, c.depIndex)].graph
                                      .nodes[c.nodeIndex].label.which)
                            if c_label not in [graph_api_capnp.Graph.Node.Label.contextDef,
                                               graph_api_capnp.Graph.Node.Label.contextAssum]:
                                raise Exception(
                                    f"{fname}: ctx {state.context} of a state {state} "
                                    f"has the wrong node classification {c_label}")

                    file_tactic_arguments.setdefault(ident, len(outcome.tacticArguments))
                    if file_tactic_arguments[ident] != len(outcome.tacticArguments):
                        raise Exception(f"{fname}: Tactic with two different argument lengths detected")
                    outcomes += 1
                    if definition.status.which() == graph_api_capnp.Definition.Status.original:
                        original_outcomes += 1
                    for a in outcome.tacticArguments:
                        if a.which == 'unresolvable':
                            unresolvable += 1
                        elif a.which == "term":
                            if (a.term.nodeIndex >=
                                node_counts[data.local_to_global(graphid, a.term.depIndex)]):
                                raise Exception(f"{fname}: Argument of outcome {outcome} of definition "
                                                f"{definition.name} is out of bounds")
                        else:
                            raise Exception(f"{fname}: unknown tactical argument {a}")
            proofs += 1
            if definition.status.which == graph_api_capnp.Definition.Status.original:
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
        results = pool.map(process1_partial, file_list)

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
