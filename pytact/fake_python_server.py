import contextlib
import sys
import socket
import argparse
import pytact.graph_visualize as gv
import capnp
import pytact.graph_api_capnp as graph_api_capnp
from pytact.data_reader import online_definitions_initialize, online_data_predict, capnp_message_generator

def text_prediction_loop(messages_generator):
    tactics = [ 'idtac "is it working?"', 'idtac "yes it is working!"', 'auto' ]
    for msg in messages_generator:
        if msg.is_predict:
            print(msg.predict.state.text)
            preds = [
                {'tacticText': t,
                'confidence': 0.5} for t in tactics ]
            response = graph_api_capnp.PredictionProtocol.Response.new_message(textPrediction=preds)
        elif msg.is_synchronize:
            print(msg)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
        elif msg.is_initialize:
            print(msg)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
        elif msg.is_check_alignment:
            alignment = {'unalignedTactics': [],
                         'unalignedDefinitions': []}
            response = graph_api_capnp.PredictionProtocol.Response.new_message(alignment=alignment)
        else:
            raise Exception("Capnp protocol error")
        print(response)
        messages_generator.send(response)

def prediction_loop(definitions, tactics, messages_generator):
    for msg in messages_generator:
        if msg.is_predict:
            with online_data_predict(
                    definitions,
                    msg.predict) as proof_state:
                gv.visualize_proof_state(proof_state)
                singleArgs = [t.ident for t in tactics if t.parameters == 0]
                preds = [{'tactic': {'ident': t}, 'arguments': [], 'confidence': 0.5} for t in singleArgs]
                if len(proof_state.context) > 0:
                    oneArg = [t.ident for t in tactics if t.parameters == 1]
                    hyp_node = proof_state.context[0]
                    preds2 = [
                        {'tactic': {'ident': t},
                         'arguments': [{'term' : {'depIndex': 0, 'nodeIndex': hyp_node.nodeid}}],
                         'confidence': 0.5} for t in oneArg ]
                    preds += preds2
                for d in definitions.definitions:
                    if d.name == "Coq.Init.Logic.I":
                        oneArg = [t.ident for t in tactics if t.parameters == 1]
                        preds2 = [
                            {'tactic': {'ident': t },
                             'arguments': [{'term' : {'depIndex': 1, 'nodeIndex': d.node.nodeid}}],
                             'confidence': 0.5} for t in oneArg ]
                        preds += preds2
                response = graph_api_capnp.PredictionProtocol.Response.new_message(prediction=preds)
                print(response)
                messages_generator.send(response)
        else:
            return msg

def graph_initialize_loop(messages_generator):
    msg = next(messages_generator, None)
    while msg is not None:
        if msg.is_predict:
            raise Exception('Predict message received without a preceding initialize message')
        elif msg.is_synchronize:
            print(msg)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
            print(response)
            messages_generator.send(response)
            msg = next(messages_generator, None)
        elif msg.is_check_alignment:
            check_alignment = msg.check_alignment
            with online_definitions_initialize(
                    check_alignment.graph,
                    check_alignment.representative) as definitions:
                for cluster in definitions.clustered_definitions:
                    print('cluster:')
                    for d in cluster:
                        print(f'    {d.name}')
                for t in check_alignment.tactics:
                    print(t)
                alignment = {'unalignedTactics': [ t.ident for t in check_alignment.tactics],
                            'unalignedDefinitions': [d.node.nodeid for d in definitions.definitions]}
                response = graph_api_capnp.PredictionProtocol.Response.new_message(alignment=alignment)
                messages_generator.send(response)
                msg = next(messages_generator, None)
        elif msg.is_initialize:
            print('---------------- New prediction context -----------------')
            init = msg.initialize
            with online_definitions_initialize(init.graph, init.representative) as definitions:
                for cluster in definitions.clustered_definitions:
                    print('cluster:')
                    for d in cluster:
                        print(f'    {d.name}')
                for t in init.tactics:
                    print(t)
                print(init.log_annotation)
                response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
                print(response)
                messages_generator.send(response)
                msg = prediction_loop(definitions, init.tactics,
                                      messages_generator)
        else:
            raise Exception("Capnp protocol error")

def run_session(args, capnp_socket, record_file):
    messages_generator = capnp_message_generator(capnp_socket, record_file)
    if args.mode == 'text':
        print('Python server running in text mode')
        text_prediction_loop(messages_generator)
    elif args.mode == 'graph':
        print('Python server running in graph mode')
        graph_initialize_loop(messages_generator)
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
