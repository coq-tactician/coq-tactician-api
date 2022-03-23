"""
simulation of python server
"""
import typing
import sys
import time

import socket

import capnp
import pytact.common
import pytact.graph_visualize as gv
import argparse

capnp.remove_import_hook()

Tactic = typing.NewType('Tactic', object)

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

DELAY = 0.0

SELECTED_HASHES = {504067591}

def debug(*args):
    print("PYTHON: ", *args)


def prediction_loop(reader, sock, tacs: list[Tactic], graph1, definitions):
    debug("entering prediction loop")
    cnt = 0

    selected_graph1_nodes = [node_idx for node_idx in definitions if graph1.nodes[node_idx].label.definition.hash in SELECTED_HASHES]
    print(selected_graph1_nodes)
    while True:
        msg = next(reader)
        msg_type = msg.which()
        if msg_type == "predict":
            gv.visualize(msg.predict.graph, msg.predict.state, graph1=graph1, filename=f'predict{cnt}')
            cnt+=1
            preds = []
            for tac in tacs:
                if tac.parameters == 0:
                    preds.append({'tactic': {'ident': tac.ident, 'arguments': []}, 'confidence': 0.5})
                elif tac.parameters == 1:
                    for hyp_node in msg.predict.state.context:
                        preds.append({'tactic': {'ident': tac.ident, 'arguments': [{'term' : {'depIndex': 0, 'nodeIndex': hyp_node}}]}, 'confidence': 0.5})
                    for hyp_node in selected_graph1_nodes:
                        preds.append({'tactic': {'ident': tac.ident, 'arguments': [{'term' : {'depIndex': 1, 'nodeIndex': hyp_node}}]}, 'confidence': 0.5})
            debug("predictions", preds)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(prediction=preds)
            debug(response)
            response.write_packed(sock)
            time.sleep(DELAY)
        elif msg_type == "synchronize":
            debug(msg)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
            debug(response)
            response.write_packed(sock)
        elif msg_type == "initialize":
            return msg
        else:
            raise Exception("Capnp protocol error in prediction_loop: "
                            "msg type is not 'predict', 'synchronize', or 'initialize'")


def initialize_loop(reader, sock):
    msg = next(reader)
    msg_type = msg.which()
    if msg_type == "initialize":
        while True:
            debug('---------------- New prediction context -----------------')
            gv.visualize_defs(msg.initialize.graph, msg.initialize.definitions, filename='initialize')
            debug("initialize tactics", msg.initialize.tactics)
            debug("initialize definitions", msg.initialize.definitions)
            tacs = list(msg.initialize.tactics)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
            debug("sending initialize response", response)
            response.write_packed(sock)
            time.sleep(DELAY)
            msg = prediction_loop(reader, sock, tacs, msg.initialize.graph, msg.initialize.definitions)
    elif msg_type == "synchronize":
        debug(msg)
        response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
        debug("sending synchronize response", response)
        response.write_packed(sock)
        initialize_loop(reader, sock)
    else:
        raise Exception("Capnp protocol error in initialize_loop: initial msg is not 'initialize' or 'synchronize'")


def main():
    parser = argparse.ArgumentParser(
        description='dummy tactic prediction python server')

    parser.add_argument('--tcp', action='store_true',
                        help='start python server on tcp/ip socket')

    parser.add_argument('--port', type=int,
                        default=33333,
                        help='run python server on this port')
    parser.add_argument('--host', type=str,
                        default='127.0.0.1',
                        help='run python server on this local ip')

    args = parser.parse_args()
    if not args.tcp:
        debug("starting stdin server")
        sock = socket.socket(fileno=sys.stdin.fileno())
    else:
        debug("starting tcp/ip server on port", args.port)
        server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_sock.bind((args.host, args.port))
        server_sock.listen(1)
        debug("tcp/ip server is listening on", args.port)
        sock, remote_addr = server_sock.accept()
        debug("coq client connected ", remote_addr)

    reader = graph_api_capnp.PredictionProtocol.Request.read_multiple_packed(sock, traversal_limit_in_words=2**64-1)
    initialize_loop(reader, sock)





if __name__ == '__main__':
    main()
