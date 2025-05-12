import contextlib
import sys
import socket
import socketserver
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
    cool_definitions = [ d.node for d in context.definitions.definitions() if d.name == "Coq.Init.Logic.I" ]
    zeroArgs = [t.ident for t in context.tactics if t.parameters == 0]
    oneArg = [t.ident for t in context.tactics if t.parameters == 1]
    for msg in prediction_requests:
        if isinstance(msg, ProofState):
            proof_state = msg
            gv.visualize_proof_state(proof_state)
            preds = [TacticPredictionGraph(t, [], 0.5) for t in zeroArgs]
            if len(proof_state.context) > 0:
                hyp_node = proof_state.context[0]
                preds += [TacticPredictionGraph(t, [hyp_node], 0.5) for t in oneArg]
            for d in cool_definitions:
                preds += [TacticPredictionGraph(t, [d], 0.5) for t in oneArg]
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
    sys.setrecursionlimit(10000)
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
            class Handler(socketserver.BaseRequestHandler):
                def handle(self):
                    run_session(args, self.request, record_file)
            class Server(socketserver.ThreadingTCPServer):
                def __init__(self, *kwargs):
                    self.allow_reuse_address = True
                    self.daemon_threads = True
                    super().__init__(*kwargs)
            addr = ('localhost', args.tcp)
            with Server(addr, Handler) as server:
                server.daemon_threads = True
                server.serve_forever()
        else:
            capnp_socket = socket.socket(fileno=sys.stdin.fileno())
            run_session(args, capnp_socket, record_file)

if __name__ == '__main__':
    main()
