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


def prediction_loop_text(r, s):
    tactics = [ 'idtac "is it working?"', 'idtac "yes it is working!"', 'auto' ]
    while True:
        g = next(r)
        msg_type = g.which()
        if msg_type == "predict":
            print(g.predict.state.text)
            preds = [
                {'tacticText': t,
                 'confidence': 0.5} for t in tactics ]
            response = graph_api_capnp.PredictionProtocol.Response.new_message(textPrediction=preds)
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

def prediction_loop(r, s, tacs, graph1, definitions):
    while True:
        g = next(r)
        msg_type = g.which()
        if msg_type == "predict":
            gv.visualize(g.predict.graph, g.predict.state, graph1=graph1)
            singleArgs = [t.ident for t in tacs if t.parameters == 0]
            preds = [{'tactic': {'ident': t}, 'arguments': [], 'confidence': 0.5} for t in singleArgs]
            if len(g.predict.state.context) > 0:
                oneArg = [t.ident for t in tacs if t.parameters == 1]
                hyp_node = g.predict.state.context[0]
                preds2 = [
                    {'tactic': {'ident': t},
                     'arguments': [{'term' : {'depIndex': 0, 'nodeIndex': hyp_node}}],
                     'confidence': 0.5} for t in oneArg ]
                preds += preds2
            for ni in definitions:
                # This definition is I, the constructor of True
                if graph1.nodes[ni].label.definition.hash == 504067591:
                    oneArg = [t.ident for t in tacs if t.parameters == 1]
                    preds2 = [
                        {'tactic': {'ident': t },
                         'arguments': [{'term' : {'depIndex': 1, 'nodeIndex': ni}}],
                         'confidence': 0.5} for t in oneArg ]
                    preds += preds2
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
            print("Capnp protocol error")
            raise Exception

def initialize_loop(r, s, textmode):
    g = next(r)
    msg_type = g.which()
    if msg_type == "initialize":
        while True:
            print('---------------- New prediction context -----------------')
            if not textmode:
                gv.visualize_defs(g.initialize.graph, g.initialize.definitions)
                print(g.initialize.tactics)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
            response.write_packed(s)
            print(response)
            import time
            time.sleep(1)
            if textmode:
                g = prediction_loop_text(r, s)
            else:
                tacs = list(g.initialize.tactics)
                g = prediction_loop(r, s, tacs, g.initialize.graph, g.initialize.definitions)
    elif msg_type == "synchronize":
        print(g)
        response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=g.synchronize)
        print(response)
        response.write_packed(s)
        initialize_loop(r, s, textmode)
    else:
        print("Capnp protocol error")
        raise Exception

def main():
    s = socket.socket(fileno=sys.stdin.fileno())
    if sys.argv[1] == 'text':
        print('Python server running in text mode')
        textmode = True
    elif sys.argv[1] == 'graph':
        print('Python server running in graph mode')
        textmode = False
    r = graph_api_capnp.PredictionProtocol.Request.read_multiple_packed(s, traversal_limit_in_words=2**64-1)
    initialize_loop(r, s, textmode)
>>>>>>> origin/coq8.11

if __name__ == '__main__':
    main()
