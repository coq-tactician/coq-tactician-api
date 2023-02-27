import contextlib
import sys
import socket
import argparse
import pytact.graph_visualize as gv
from pytact.data_reader import (capnp_message_generator, ProofState,
                                TacticPredictionGraph, TacticPredictionsGraph,
                                TacticPredictionText, TacticPredictionsText,
                                GlobalContextMessage, CheckAlignmentMessage, CheckAlignmentResponse)

def text_prediction_loop(context : GlobalContextMessage):
    tactics = [ 'idtac "is it working?"', 'idtac "yes it is working!"', 'auto' ]
    prediction_requests = context.prediction_requests
    for msg in prediction_requests:
        if isinstance(msg, ProofState):
            proof_state = msg
            print(proof_state.text)
            preds = [TacticPredictionText(t, 0.5) for t in tactics]
            prediction_requests.send(TacticPredictionsText(preds))
        elif isinstance(msg, CheckAlignmentMessage):
            alignment = CheckAlignmentResponse([], [])
            prediction_requests.send(alignment)
        elif isinstance(msg, GlobalContextMessage):
            text_prediction_loop(msg)
        else:
            raise Exception("Capnp protocol error")

def graph_initialize_loop(context : GlobalContextMessage, level):
    print(f"level {level}")
    for cluster in context.definitions.clustered_definitions(full = False):
        print('cluster:')
        for d in cluster:
            print(f'    {d.name}')
    for t in context.tactics:
        print(t)
    print(context.log_annotation)
    prediction_requests = context.prediction_requests
    for msg in prediction_requests:
        if isinstance(msg, ProofState):
            proof_state = msg
            gv.visualize_proof_state(proof_state)
            zeroArgs = [t.ident for t in context.tactics if t.parameters == 0]
            preds = [TacticPredictionGraph(t, [], 0.5) for t in zeroArgs]
            if len(proof_state.context) > 0:
                oneArg = [t.ident for t in context.tactics if t.parameters == 1]
                hyp_node = proof_state.context[0]
                preds += [TacticPredictionGraph(t, [hyp_node], 0.5) for t in oneArg]
            for d in context.definitions.definitions():
                if d.name == "Coq.Init.Logic.I":
                    oneArg = [t.ident for t in context.tactics if t.parameters == 1]
                    preds += [TacticPredictionGraph(t, [d.node], 0.5) for t in oneArg]
            prediction_requests.send(TacticPredictionsGraph(preds))
        elif isinstance(msg, CheckAlignmentMessage):
            unknown_definitions = list(context.definitions.definitions())
            unknown_tactics = [t.ident for t in context.tactics]
            alignment = CheckAlignmentResponse(unknown_definitions, unknown_tactics)
            prediction_requests.send(alignment)
        elif isinstance(msg, GlobalContextMessage):
            graph_initialize_loop(msg, level + 1)
        else:
            raise Exception("Capnp protocol error")

def run_session(args, capnp_socket, record_file):
    messages_generator = capnp_message_generator(capnp_socket, record_file)
    if args.mode == 'text':
        print('Python server running in text mode')
        text_prediction_loop(messages_generator)
    elif args.mode == 'graph':
        print('Python server running in graph mode')
        graph_initialize_loop(messages_generator, 0)
    else:
        raise Exception("The 'mode' argument needs to be either 'text' or 'graph'")

def main():
    parser = argparse.ArgumentParser(
        description = "Example python server capable of communicating with Coq through Tactician's 'synth' tactic",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('mode',
                        type=str,
                        choices=['graph', 'text'],
                        help='"graph" to communicate in graph-mode, "text" to communicate in text-mode')
    parser.add_argument('--tcp',
                        type = int,
                        default = 0,
                        help='Run in tcp mode instead of stdin mode on the specified port.')
    parser.add_argument('--record',
                        dest="record_file",
                        type = str,
                        default = None,
                        help='Record all exchanged messages to the specified file, so that they can later be ' +
                        'replayed through "pytact-fake-coq"')
    args = parser.parse_args()

    if args.record_file is not None:
        record_context = open(args.record_file, 'wb')
    else:
        record_context = contextlib.nullcontext()
    with record_context as record_file:
        if args.tcp != 0:
            addr = ('localhost', args.tcp)
            server_sock = socket.create_server(addr)
            try:
                while True:
                    capnp_socket, remote_addr = server_sock.accept()
                    print(f"coq client connected {remote_addr}")
                    run_session(args, capnp_socket, record_file)
            finally:
                print(f'closing the server on port {addr[1]}')
                server_sock.close()
        else:
            capnp_socket = socket.socket(fileno=sys.stdin.fileno())
            run_session(args, capnp_socket, record_file)

if __name__ == '__main__':
    main()
