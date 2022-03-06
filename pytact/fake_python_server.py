import os
import sys

import socket

import capnp
import pytact.common
import pytact.graph_visualize as gv
capnp.remove_import_hook()

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

def prediction_loop(r, s, tacs):
    while True:
        g = next(r)
        msg_type = g.which()
        if msg_type == "predict":
            gv.visualize(g.predict.graph, g.predict.state)
            preds = [{'tactic': {'ident': t, 'arguments': []}, 'confidence': 0.5} for t in tacs]
            response = graph_api_capnp.PredictionProtocol.Response.new_message(prediction=preds)
            print(response)
            response.write_packed(s)
            import time
            time.sleep(1)
        elif msg_type == "synchronize":
            print(g)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=g.synchronize)
            print(response)
            response.write_packed(s)
        elif msg_type == "initialize":
            return g
        else:
            print("Capnp protocol error")
            raise Exception

def initialize_loop(r, s):
    g = next(r)
    msg_type = g.which()
    if msg_type == "initialize":
        while True:
            print('---------------- New prediction context -----------------')
            gv.visualize_defs(g.initialize.graph, g.initialize.definitions)
            print(g.initialize.tactics)
            tacs = list(g.initialize.tactics)
            singleArgs = [t.ident for t in tacs if t.parameters == 0]
            response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
            response.write_packed(s)
            print(response)
            import time
            time.sleep(5)
            g = prediction_loop(r, s, singleArgs)
    elif msg_type == "synchronize":
        print(g)
        response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=g.synchronize)
        print(response)
        response.write_packed(s)
        initialize_loop(r, s)
    else:
        print("Capnp protocol error")
        raise Exception

def main():
    s = socket.socket(fileno=sys.stdin.fileno())
    r = graph_api_capnp.PredictionProtocol.Request.read_multiple_packed(s, traversal_limit_in_words=2**64-1)
    initialize_loop(r, s)

if __name__ == '__main__':
    main()
