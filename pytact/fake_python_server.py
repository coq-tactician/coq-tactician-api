import sys
import socket
import pytact.graph_visualize as gv
import capnp
import pytact.graph_api_capnp as graph_api_capnp
from pytact.data_reader import online_data_initialize, online_data_predict
from pytact.graph_api_capnp_cython import PredictionProtocol_Request_Initialize_Reader, PredictionProtocol_Request_Predict_Reader

def text_prediction_loop(r, s):
    tactics = [ 'idtac "is it working?"', 'idtac "yes it is working!"', 'auto' ]
    for g in r:
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
            print(g)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
            print(response)
            response.write_packed(s)
        else:
            raise Exception("Capnp protocol error")

def prediction_loop(init, r, s):
    for msg in r:
        msg_type = msg.which()
        if msg_type == "predict":
            with online_data_predict(init, PredictionProtocol_Request_Predict_Reader(msg.predict)) as proof_state:
                gv.visualize_proof_state(proof_state)
                singleArgs = [t.ident for t in init.tactics if t.parameters == 0]
                preds = [{'tactic': {'ident': t}, 'arguments': [], 'confidence': 0.5} for t in singleArgs]
                if len(proof_state.context) > 0:
                    oneArg = [t.ident for t in init.tactics if t.parameters == 1]
                    hyp_node = proof_state.context[0]
                    preds2 = [
                        {'tactic': {'ident': t},
                         'arguments': [{'term' : {'depIndex': 0, 'nodeIndex': hyp_node}}],
                         'confidence': 0.5} for t in oneArg ]
                    preds += preds2
                for d in init.definitions:
                    if d.name == "Coq.Init.Logic.I":
                        oneArg = [t.ident for t in init.tactics if t.parameters == 1]
                        preds2 = [
                            {'tactic': {'ident': t },
                             'arguments': [{'term' : {'depIndex': 1, 'nodeIndex': d.node.nodeid}}],
                             'confidence': 0.5} for t in oneArg ]
                        preds += preds2
                response = graph_api_capnp.PredictionProtocol.Response.new_message(prediction=preds)
                print(response)
                response.write_packed(s)
                import time
                time.sleep(1)
        elif msg_type == "synchronize":
            print(msg)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(synchronized=msg.synchronize)
            print(response)
            response.write_packed(s)
        elif msg_type == "checkAlignment":
            print(msg)
            alignment = {'unalignedTactics': [ t.ident for t in msg.checkAlignment.tactics],
                         'unalignedDefinitions': list(msg.checkAlignment.definitions)}
            response = graph_api_capnp.PredictionProtocol.Response.new_message(alignment=alignment)
            response.write_packed(s)
        elif msg_type == "initialize":
            return msg
        else:
            raise Exception("Capnp protocol error")

def graph_initialize_loop(r, s):
    init = graph_api_capnp.PredictionProtocol.Request.new_message(initialize={
        'tactics': [],
        'graph': {'nodes': [], 'edges': []},
        'definitions': [],
        'logAnnotation': ""
    }).as_reader()
    while init is not None:
        print('---------------- New prediction context -----------------')
        with online_data_initialize(PredictionProtocol_Request_Initialize_Reader(init.initialize)) as base:
            for d in base.definitions:
                print(d.name)
            for t in base.tactics:
                print(t)
            print(base.log_annotation)
            response = graph_api_capnp.PredictionProtocol.Response.new_message(initialized=None)
            print(response)
            response.write_packed(s)
            init = prediction_loop(base, r, s)
        import time
        time.sleep(1)

def main():
    s = socket.socket(fileno=sys.stdin.fileno())
    r = graph_api_capnp.PredictionProtocol.Request.read_multiple_packed(s, traversal_limit_in_words=2**64-1)
    if sys.argv[1] == 'text':
        print('Python server running in text mode')
        text_prediction_loop(r, s)
    elif sys.argv[1] == 'graph':
        print('Python server running in graph mode')
        graph_initialize_loop(r, s)

if __name__ == '__main__':
    main()
