from collections import defaultdict
from collections.abc import Generator
from dataclasses import dataclass
from pathlib import Path
import sys
import socket
import argparse
import signal
from typing import Any, BinaryIO
import contextlib
import capnp
import pytact.graph_api_capnp as graph_api_capnp
from pytact.data_reader import online_definitions_initialize, online_data_predict, data_reader, Original, capnp_message_generator

@dataclass
class GlobalArgument:
    identity: int
@dataclass
class LocalArgument:
    context_index: int
@dataclass
class OracleTactic:
    tactic_id: int
    arguments: list[GlobalArgument | LocalArgument]

def text_prediction_loop(text_oracle_data, messages_generator):
    with contextlib.suppress(StopIteration):
        msg = next(messages_generator)
        while True:
            if msg.is_predict:
                if msg.predict.state.text in text_oracle_data:
                    preds = [
                    {'tacticText': text_oracle_data[msg.predict.state.text][0],
                    'confidence': 1} ]
                else:
                    preds = []
                response = graph_api_capnp.PredictionProtocol.Response.new_message(textPrediction=preds)
            elif msg.is_initialize:
                response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
            elif msg.is_synchronize:
                response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
            elif msg.is_check_alignment:
                alignment = {'unalignedTactics': [],
                            'unalignedDefinitions': []}
                response = graph_api_capnp.PredictionProtocol.Response.new_message(alignment=alignment)
            else:
                raise Exception("Capnp protocol error")
            msg = messages_generator.send(response)

def prediction_loop(msg, oracle_data, definitions, tactics, messages_generator):
    available_tacticids = set([ t.ident for t in tactics ])
    available_definitions = { d.node.identity : d.node.nodeid for d in definitions.definitions }
    while True:
        if msg.is_predict:
            with online_data_predict(
                    definitions,
                    msg.predict) as proof_state:
                preds = []
                for tactic in oracle_data[proof_state.root.identity]:
                    if tactic.tactic_id in available_tacticids:
                        complete = True
                        resolved_args = []
                        for arg in tactic.arguments:
                            if isinstance(arg, LocalArgument):
                                resolved_args.append(
                                    {'term': {'depIndex': 0,
                                              'nodeIndex': proof_state.context[arg.context_index].nodeid}})
                            elif isinstance(arg, GlobalArgument) and arg.identity in available_definitions:
                                resolved_args.append(
                                    {'term': {'depIndex': 1,
                                              'nodeIndex': available_definitions[arg.identity]}})
                            else:
                                complete = False
                        if complete:
                            preds = [{'tactic': {'ident': tactic.tactic_id},
                                      'arguments': resolved_args,
                                      'confidence': 1}]
                            break
                response = graph_api_capnp.PredictionProtocol.Response.new_message(prediction=preds)
                msg = messages_generator.send(response)
        else:
            return msg

def graph_initialize_loop(oracle_data, known_definitions, known_tactics, messages_generator):
    with contextlib.suppress(StopIteration):
        msg = next(messages_generator)
        while True:
            if msg.is_predict:
                raise Exception('Predict message received without a preceding initialize message')
            elif msg.is_synchronize:
                response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
                msg = messages_generator.send(response)
            elif msg.is_check_alignment:
                check_alignment = msg.check_alignment
                with online_definitions_initialize(
                        check_alignment.graph,
                        check_alignment.representative) as definitions:
                    unknown_definitions = [ d.node.nodeid for d in definitions.definitions
                                            if d.node.identity not in known_definitions ]
                    unknown_tactics = [ t.ident for t in check_alignment.tactics
                                        if t.ident not in known_tactics ]
                    alignment = {'unalignedTactics': unknown_tactics,
                                'unalignedDefinitions': unknown_definitions}
                    response = graph_api_capnp.PredictionProtocol.Response.new_message(alignment=alignment)
                    msg = messages_generator.send(response)
            elif msg.is_initialize:
                init = msg.initialize
                with online_definitions_initialize(init.graph, init.representative) as definitions:
                    response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
                    msg = prediction_loop(messages_generator.send(response),
                                          oracle_data, definitions, init.tactics, messages_generator)
            else:
                raise Exception("Capnp protocol error")

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
    oracle_data = defaultdict(list)
    text_oracle_data = defaultdict(list)
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
                            text_oracle_data[outcome.before.text].append(outcome.tactic.text_non_anonymous)
                            tactic_args = outcome.tactic_arguments
                            if any(arg is None for arg in tactic_args):
                                continue # If an argument is unknown we are screwed
                            args = []
                            for arg in tactic_args:
                                if arg_def := arg.definition:
                                    args.append(GlobalArgument(arg_def.node.identity))
                                else:
                                    args.append(LocalArgument(list(outcome.before.context).index(arg)))
                            oracle_tactic = OracleTactic(outcome.tactic.ident, args)
                            oracle_data[outcome.before.root.identity].append(oracle_tactic)
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
