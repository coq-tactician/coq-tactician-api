from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
import sys
import socket
import socketserver
import argparse
import contextlib
from typing import Union, Tuple
from pytact.data_reader import (
    data_reader, Original, capnp_message_generator, ProofState,
    TacticPredictionGraph, TacticPredictionsGraph,
    TacticPredictionText, TacticPredictionsText,
    GlobalContextMessage, CheckAlignmentMessage, CheckAlignmentResponse,
    Node)

from immutables import Map

def forall_spine_length(node: Node):
    label = node.label
    if label.is_prod:
        return 1 + forall_spine_length(node.children[1][1])
    else:
        return 0

def lambda_spine_length(node: Node):
    label = node.label
    if label.is_lambda_:
        return 1 + lambda_spine_length(node.children[1][1])
    else:
        return 0

def remove_forall_spine(node: Node, length: int, depth, indices):
    if length == 0:
        return node, indices
    else:
        assert node.label.is_prod
        return remove_forall_spine(node.children[1][1], length - 1, depth + 1, indices.set(node, depth))

def remove_lambda_spine(node: Node, length: int, depth, indices):
    if length == 0:
        return node, indices
    else:
        assert node.label.is_lambda_
        return remove_lambda_spine(node.children[1][1], length - 1, depth + 1, indices.set(node, depth))

def head(node: Node):
    if node.label.is_app:
        return head(node.children[0][1])
    else:
        return node

def print_branch(branch, depth, indices, parameters):
    constr, trm = (child for _, child in branch.children)
    arguments = forall_spine_length(constr.children[0][1])
    binders = arguments - parameters
    params = ' '.join(parameters * '_')
    args = ' '.join([f"v{i}" for i in range(depth, depth + binders)])

    constr_str = print_graph_imp(constr, depth, indices)
    trm_str = print_graph_imp(trm, depth, indices)

    # Here we essentially do a big eta-expansion of `args`.
    # Normally, it would be better to just remove a sufficient
    # number of binders from `trm`. However, this doesn't always work.
    # The kernel's representation is more expressive than the user-facing
    # representation. Namely, in user-land, all variables in `args` have
    # to be explicitly listed (matched). But in kernel-land, the conclusion
    # of the match just needs to be any function that can be applied
    # to the appropriate number of match arguments. That function does not
    # need to syntactically be a lambda.

    # Example:
    # - Kernel-land: case (x: A /\ B) first
    # - User-land translation: match (x: A /\ B) with | conj a b => first a b end
    # - What a human would write: match (x: A /\ B) with | conj a b => a end

    #TODO: This does not reach a stable fixpoint under repeated printing-parsing
    #      The term will get increasinly big. To remedy, implement a special case
    #      where trm has enough lambdas and eta expansion is not needed.
    return f"{constr_str} {params} {args} => ({trm_str}) {args}"

def print_ind_pattern(node: Node, indices, in_spine = True):
    label = node.label
    if label.is_app and in_spine:
        left, right = (child for _, child in node.children)
        left_str = print_ind_pattern(left, indices, True)
        right_str = print_ind_pattern(right, indices, False)
        return f"{left_str} {right_str}"
    elif label.is_definition and in_spine:
        return f"@{node.definition.name}"
    elif label.is_rel:
        binder = node.children[0][1]
        if name := indices.get(binder, None):
            return f"v{name}"
        else:
            return "_"
    else:
        return "_"

def find_first_pattern_in_spine(ind: Node, ret: Node):
    assert ret.label.is_lambda_
    type_head = head(ret.children[0][1])
    if type_head == ind:
        return 0
    else:
        return 1 + find_first_pattern_in_spine(ind, ret.children[1][1])

def ind_parameter_count_heuristic(node: Node):
    ind, ret, disc, *branches = (child for _, child in node.children)

    # Total length of the spine, to be divided into parameters and non-parameters
    ind_spine_length = forall_spine_length(ind.children[0][1])

    # The depth of the first inductive pattern in the return give a lower bound on non-parameters
    min_non_parameters = find_first_pattern_in_spine(ind, ret)

    constrs = (branch.children[0][1].children[0][1] for branch in branches)
    # Each constructor must have a product for each parameter. That gives an upper bound
    max_parameters = min((ind_spine_length,) + tuple(forall_spine_length(cstr) for cstr in constrs))

    if max_parameters + min_non_parameters <= ind_spine_length:
        return max_parameters, ind_spine_length - max_parameters
    else:
        # We can't infer with certainty. Assume min_non_parameters is correct and warn
        print(f"Warn: Can't infer parameter count of {ind.definition.name}: {ind_spine_length} = {max_parameters} + {min_non_parameters}", file=sys.stderr)
        return ind_spine_length - min_non_parameters, min_non_parameters

def print_case(node: Node, depth, indices):
    ind, ret, disc, *branches = (child for _, child in node.children)
    disc = print_graph_imp(disc, depth, indices)

    parameters, non_parameters = ind_parameter_count_heuristic(node)

    ret_rem, extra_indices = remove_lambda_spine(ret, non_parameters, depth, indices)
    assert ret_rem.label.is_lambda_

    extra_indices_only = Map({n:b for n,b in extra_indices.items() if b >= depth})
    in_cls = print_ind_pattern(ret_rem.children[0][1], extra_indices_only)
    discr_var = depth + non_parameters
    ret_cls = print_graph_imp(
        ret_rem.children[1][1],
        discr_var + 1,
        extra_indices.set(ret_rem, discr_var))

    branches = [print_branch(branch, depth, indices, parameters) for branch in branches]
    return f"match ({disc}) as v{discr_var} in ({in_cls}) return ({ret_cls}) with {' | '.join(branches)} end"

def print_fix_fun(node: Node, rec: int, depth, indices):
    typ, trm = (child for _, child in node.children)
    typ_spine_length = forall_spine_length(typ)
    trm_spine_length = lambda_spine_length(trm)
    spine_length = min(typ_spine_length, trm_spine_length)
    trm_rem, new_indices = remove_lambda_spine(trm, spine_length, depth, Map())
    typ_rem, _ = remove_forall_spine(typ, spine_length, depth, Map())
    binders = sorted(new_indices.items(), key=lambda kv: kv[1])
    parameters = ' '.join(f"(v{i} : {print_graph_imp(node.children[0][1], depth, indices)})" for node, i in binders)
    trm_rem_str = print_graph_imp(trm_rem, depth + trm_spine_length, indices.update(new_indices))
    typ_str = print_graph_imp(typ_rem, depth + trm_spine_length, indices.update(new_indices))
    return f"v{rec} {parameters} : ({typ_str}) := {trm_rem_str}"

def print_graph_imp(node: Node, depth, indices):

    label = node.label
    if label.is_sort_prop:
        return "Prop"
    elif label.is_sort_set:
        return "Set"
    elif label.is_sort_type:
        return "Type"
    elif label.is_sort_s_prop:
        return "SProp"
    elif label.is_prod:
        left = print_graph_imp(node.children[0][1], depth, indices)
        right = print_graph_imp(node.children[1][1], depth + 1, indices.set(node, depth))
        return f"forall (v{depth} : {left}), ({right})"
    if label.is_lambda_:
        left = print_graph_imp(node.children[0][1], depth, indices)
        right = print_graph_imp(node.children[1][1], depth + 1, indices.set(node, depth))
        return f"fun (v{depth} : {left}) => ({right})"
    elif label.is_app:
        left, right = (print_graph_imp(child, depth, indices) for _, child in node.children)
        return f"({left}) ({right})"
    elif label.is_definition:
        return f"@{node.definition.name}"
    elif label.is_case:
        return print_case(node, depth, indices)
    elif label.is_rel:
        binder = node.children[0][1]
        name = indices.get(binder, '-unknown')
        return f"v{name}"
    elif label.is_cast:
        trm, typ = (print_graph_imp(child, depth, indices) for _, child in node.children)
        return f"({trm}) : ({typ})"
    elif label.is_let_in:
        # children = (print_graph_imp(child, depth, indices) for _, child in node.children)
        # return f"letin {' * '.join(children)}"
        typ, trm_def, trm_in = (child for _, child in node.children)
        typ = print_graph_imp(typ, depth, indices)
        trm_def = print_graph_imp(trm_def, depth, indices)
        trm_in = print_graph_imp(trm_in, depth + 1, indices.set(node, depth))
        return f"let v{depth} : ({typ}) := ({trm_def}) in ({trm_in})"
    elif label.is_fix:
        ret, *fixfuns = (child for _, child in node.children)
        indices = indices.update({fixfun: depth + n for n, fixfun in enumerate(fixfuns)})
        fixfuns_str = (print_fix_fun(fixfun, depth + i, depth + len(fixfuns), indices)
                       for i, fixfun in enumerate(fixfuns))
        return f"fix {' * '.join(fixfuns_str)}"


    else:
        return str(label.which.name)

def print_graph(node: Node):
    return print_graph_imp(node, 0, Map())

def graph_prediction_loop(context: GlobalContextMessage, oracle_data):
    prediction_requests = context.prediction_requests
    for msg in prediction_requests:
        if isinstance(msg, ProofState):
            to_prove = msg.root.children[0][1]
            print(to_prove.identity)
            print(f"Proving {print_graph(to_prove)}")
            if proof := oracle_data.get(to_prove.identity, None):
                proof_txt = print_graph(proof)
                print(f"proof: {proof_txt}")
                possible_tactics = [ TacticPredictionText(f"refine ({proof_txt})", 1) ]
            else:
                possible_tactics = [ TacticPredictionText(f"repeat match goal with H : _ |- _ => revert H end", 1) ]
            prediction_requests.send(TacticPredictionsText(possible_tactics))
        elif isinstance(msg, CheckAlignmentMessage):
            alignment = CheckAlignmentResponse([], [])
            prediction_requests.send(alignment)
        elif isinstance(msg, GlobalContextMessage):
            graph_prediction_loop(msg, oracle_data)
        else:
            raise Exception("Capnp protocol error")

def run_session(oracle_data, capnp_socket, record_file):
    messages_generator = capnp_message_generator(capnp_socket, record_file)
    print('Python server running in graph mode')
    graph_prediction_loop(messages_generator, oracle_data)

def main():
    sys.setrecursionlimit(10000)
    parser = argparse.ArgumentParser(
        description = 'A tactic prediction server acting as an oracle, retrieving it\'s information from a dataset',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('dataset',
                        type=str,
                        help=('The location of the dataset from which to extract the oracle information. ' +
                              'Either a dataset directory, or a SquashFS image, ' +
                              'which will be automatically mounted.'))
    parser.add_argument('--tcp',
                        dest='port',
                        type = int,
                        default = None,
                        help='Run in tcp mode instead of stdin mode on the specified port.')
    parser.add_argument('--record',
                        dest="record_file",
                        type = str,
                        default = None,
                        help='Record all exchanged messages to the specified file, so that they can later be ' +
                        'replayed through "pytact-fake-coq"')
    cmd_args = parser.parse_args()

    print("Building oracle data...")
    dataset_path = Path(cmd_args.dataset).resolve()
    oracle_data = defaultdict(set)
    with data_reader(dataset_path) as data:
        for datafile in data.values():
            # if datafile.filename not in [ Path('theories/Init/Logic.bin') ]:
            #     continue
            print(datafile.filename)
            count = 0
            for d in datafile.definitions():
                if not isinstance(d.status, Original):
                    continue # For an oracle, we are not interested in non-original proofs
                if d.proof:
                    print(d.name)
                    proof = d.node.children[1][1]
                    typ = d.node.children[0][1]
                    # print(d.term_text)
                    # print(print_graph(proof))
                    if typ.identity not in oracle_data:
                        oracle_data[typ.identity] = proof
                        count += 1
        print("Oracle data built, ready for incoming connections")

        if cmd_args.record_file is not None:
            record_context = open(cmd_args.record_file, 'wb')
        else:
            record_context = contextlib.nullcontext()
        with record_context as record_file:
            if cmd_args.port is not None:
                class Handler(socketserver.BaseRequestHandler):
                    def handle(self):
                        run_session(oracle_data, self.request, record_file)
                class Server(socketserver.ForkingTCPServer):
                    def __init__(self, *kwargs):
                        self.allow_reuse_address = True
                        self.daemon_threads = True
                        super().__init__(*kwargs)
                addr = ('localhost', cmd_args.port)
                with Server(addr, Handler) as server:
                    server.serve_forever()
            else:
                capnp_socket = socket.socket(fileno=sys.stdin.fileno())
                run_session(oracle_data, capnp_socket, record_file)

if __name__ == '__main__':
    main()
