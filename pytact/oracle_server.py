from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
import sys
import socket
import argparse
import contextlib
from pytact.data_reader import (data_reader, Original, capnp_message_generator, ProofState,
                                TacticPredictionGraph, TacticPredictionsGraph,
                                TacticPredictionText, TacticPredictionsText,
                                GlobalContextMessage, CheckAlignmentMessage, CheckAlignmentResponse)

@dataclass(eq=True, frozen=True)
class GlobalArgument:
    identity: int
@dataclass(eq=True, frozen=True)
class LocalArgument:
    context_index: int
@dataclass(eq=True, frozen=True)
class OracleTactic:
    tactic_id: int
    arguments: tuple[GlobalArgument | LocalArgument, ...]
    clean: bool

def text_prediction_loop(text_oracle_data, messages_generator):
    for msg in messages_generator:
        if isinstance(msg, GlobalContextMessage):
            for proof_state in msg.prediction_requests:
                if proof_state.text in text_oracle_data:
                    preds = [TacticPredictionText(t, 1) for t in text_oracle_data[proof_state.text]]
                else:
                    preds = []
                msg.prediction_requests.send(TacticPredictionsText(preds))
        elif isinstance(msg, CheckAlignmentMessage):
            alignment = CheckAlignmentResponse([], [])
            messages_generator.send(alignment)
        else:
            raise Exception("Capnp protocol error")

def prediction_loop(oracle_data, context, known_definitions, known_tactics):
    available_tacticids = set([ t.ident for t in context.tactics ])
    available_definitions = { d.node.identity : d.node for d in context.definitions.definitions }
    prediction_requests = context.prediction_requests
    for msg in prediction_requests:
        if isinstance(msg, ProofState):
            proof_state = msg
            def resolve_arg(arg):
                if isinstance(arg, LocalArgument):
                    return proof_state.context[arg.context_index]
                elif isinstance(arg, GlobalArgument) and arg.identity in available_definitions:
                    return available_definitions[arg.identity]
                else:
                    return None
            possible_tactics = [
                TacticPredictionGraph(t.tactic_id,
                                    [resolve_arg(arg) for arg in t.arguments],
                                    1 if t.clean else 0.95)
                for t in sorted(oracle_data[proof_state.root.identity], key = lambda t: not t.clean)
                if t.tactic_id in available_tacticids and
                all([resolve_arg(arg) is not None for arg in t.arguments])]
            prediction_requests.send(TacticPredictionsGraph(possible_tactics))
        elif isinstance(msg, CheckAlignmentMessage):
            unknown_definitions = [ d for d in context.definitions.definitions
                                    if d.node.identity not in known_definitions ]
            unknown_tactics = [ t.ident for t in context.tactics
                                if t.ident not in known_tactics ]
            alignment = CheckAlignmentResponse(unknown_definitions, unknown_tactics)
            prediction_requests.send(alignment)
        else:
            raise Exception("Capnp protocol error")

def graph_initialize_loop(oracle_data, known_definitions, known_tactics, messages_generator):
    for msg in messages_generator:
        prediction_loop(oracle_data, msg, known_definitions, known_tactics)

def run_session(oracle_data, text_oracle_data, known_definitions, known_tactics, args, capnp_socket, record_file):
    messages_generator = capnp_message_generator(capnp_socket, record_file)
    if args.mode == 'text':
        print('Python server running in text mode')
        text_prediction_loop(text_oracle_data, messages_generator)
    elif args.mode == 'graph':
        print('Python server running in graph mode')
        graph_initialize_loop(oracle_data, known_definitions, known_tactics, messages_generator)
    else:
        raise Exception("The 'mode' argument needs to be either 'text' or 'graph'")

def main():
    parser = argparse.ArgumentParser(
        description = 'A tactic prediction server acting as an oracle, retrieving it\'s information from a dataset',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('mode',
                        type=str,
                        choices=['graph', 'text'],
                        help='"graph" to communicate in graph-mode, "text" to communicate in text-mode')
    parser.add_argument('dataset',
                        type=str,
                        help='The location of the dataset from which to extract the oracle information')
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
    text_oracle_data = defaultdict(set)
    known_definitions = set()
    known_tactics = set()
    with data_reader(dataset_path) as data:
        for datafile in data.values():
            for d in datafile.definitions():
                known_definitions.add(d.node.identity)
                if proof := d.proof:
                    for step in proof:
                        for outcome in step.outcomes:
                            if outcome.tactic is None:
                                continue # If the tactic is unknown we are screwed
                            known_tactics.add(outcome.tactic.ident)
                            if not isinstance(d.status, Original):
                                continue # For an oracle, we are not interested in non-original proofs
                            if len(outcome.after) == 1:
                                if outcome.before.id == outcome.after[0].id:
                                    continue # This tactic didn't do anything, we can ignore it
                                if outcome.before.root.identity == outcome.after[0].root.identity:
                                    # This tactic did something, but very minimally, usually just an identity cast
                                    continue
                            text_oracle_data[outcome.before.text].add(outcome.tactic.text_non_anonymous)
                            tactic_args = outcome.tactic_arguments
                            if any(arg is None for arg in tactic_args):
                                continue # If an argument is unknown we are screwed
                            args = []
                            for arg in tactic_args:
                                if arg_def := arg.definition:
                                    args.append(GlobalArgument(arg_def.node.identity))
                                else:
                                    args.append(LocalArgument(list(outcome.before.context).index(arg)))
                            oracle_tactic = OracleTactic(outcome.tactic.ident, tuple(args),
                                                         outcome.tactic.text == outcome.tactic.interm_text)
                            oracle_data[outcome.before.root.identity].add(oracle_tactic)
    print("Oracle data built, ready for incoming connections")

    if cmd_args.record_file is not None:
        record_context = open(cmd_args.record_file, 'wb')
    else:
        record_context = contextlib.nullcontext()
    with record_context as record_file:
        if cmd_args.port is not None:
            addr = ('localhost', cmd_args.port)
            server_sock = socket.create_server(addr)
            try:
                while True:
                    capnp_socket, remote_addr = server_sock.accept()
                    print(f"coq client connected {remote_addr}")
                    run_session(oracle_data, text_oracle_data, known_definitions, known_tactics,
                                cmd_args, capnp_socket, record_file)
            finally:
                print(f'closing the server on port {addr[1]}')
                server_sock.close()
        else:
            capnp_socket = socket.socket(fileno=sys.stdin.fileno())
            run_session(oracle_data, text_oracle_data, known_definitions, known_tactics,
                        cmd_args, capnp_socket, record_file)

if __name__ == '__main__':
    main()
