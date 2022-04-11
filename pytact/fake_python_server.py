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


def debug_record(msg, fname: str):
    msg_copy = msg.as_builder()
    debug("DUMP msg to ", fname)
    with open(fname, 'wb') as f_out:
        msg_copy.write(f_out)




def process_synchronize(sock, msg):
    debug(msg)
    response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
    debug("sending synchronize response in the initialize loop", response)
    response.write_packed(sock)

def process_initialize(sock, msg):
    graph1 = msg.initialize.graph
    definitions = msg.initialize.definitions
    # gv.visualize_defs(graph1, definitions, filename='initialize')
    debug("initialize tactics", list(msg.initialize.tactics)[:10])
    debug("initialize definitions", list(msg.initialize.definitions)[:10])
    tacs = list(msg.initialize.tactics)
    response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
    debug("sending initialize response", response)
    response.write_packed(sock)
    time.sleep(DELAY)

    selected_graph1_nodes = [node_idx for node_idx in definitions
                             if graph1.nodes[node_idx].label.definition.hash in SELECTED_HASHES]
    debug(selected_graph1_nodes)

    return tacs, selected_graph1_nodes

def process_predict(sock, msg,  selected_graph1_nodes, tacs):
     # gv.visualize(msg.predict.graph, msg.predict.state, graph1=graph1, filename=f'predict{theorem_step_cnt}')
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


def main_loop(reader, sock):
    context_cnt = -1
    for msg in reader:
        msg_type = msg.which()
        debug("message: ", msg.which())
        if msg_type == "synchronize":
            process_synchronize(sock, msg)
        elif msg_type == "initialize":
            context_cnt += 1
            debug(f'### CONTEXT {context_cnt} ####')
            debug_record(msg, f'msg_init.{context_cnt}.bin')
            tacs, selected_graph1_nodes = process_initialize(sock, msg)
            theorem_cnt = 0
        elif msg_type == "predict":
            process_predict(sock, msg,  selected_graph1_nodes, tacs)
            debug_record(msg, f'msg_predict.{context_cnt}.{theorem_cnt}.bin')
            theorem_cnt += 1

        else:
            raise Exception("Capnp protocol error in prediction_loop: "
                            "msg type is not 'predict', 'synchronize', or 'initialize'")


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
    main_loop(reader, sock)





if __name__ == '__main__':
    main()
