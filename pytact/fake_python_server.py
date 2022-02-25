import os
import sys

import socket
import asyncio

from pathlib import Path

import capnp
import pytact.common
import pytact.graph_visualize as gv
capnp.remove_import_hook()

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

class PredictionContextImpl(graph_api_capnp.PredictionContext.Server):

    def __init__(self, tacs):   # currently is initiliazed with a list of tactics that have 0 arguments
        self.tacs = tacs

    def predict(self, graph, state, _context):
        '''
        graph: Graph - proof state graph (dynamical graph per proof state)
        state: ProofState (root, context, text), where context is the list of pointers to graph (text is not implemented)

        returns List(Prediction)
        Prediction is tactic + list of arguments *sorted* by decreasing confidence
        # (currently the confidence itself is not used)
        # the information is passed through the *sorted* order
        where argument is a global node
        where dep 0 stands for this local dynamical graph of the proof state
        where dep 1 stands for the global graph with which we instantiated the predictionContext

        '''
        print('New prediction request')
        gv.visualize(graph, state)
        preds = [{'tactic': {'ident': t, 'arguments': []}, 'confidence': 0.5} for t in self.tacs]
        print(preds)
        import time
        time.sleep(5)
        return preds

class PushReinforceImpl(graph_api_capnp.PushReinforce.Server):

    def predictionContext(self, available, graph, definitions, _context):
        '''

        available: AvailableTactics (interface implemented in coq)
        graph: this is the global graph
        definitions: the list of pointers to that graph
        _context: is boilerplate for capnp
        '''
        print('---------------- New prediction context -----------------')

        gv.visualize_defs(graph, definitions)

        def cont(tacs):
            print(tacs)
            tacs = list(tacs.tactics)
            singleArgs = [t.ident for t in tacs if t.parameters == 0]
            print(singleArgs)
            setattr(_context.results, 'result', PredictionContextImpl(singleArgs))    #set the result to return to coq
                                                                                      # PredictionContextImpl(singleArgs)
            def printTacs2(text, tacs):
                print(text)
                return printTacs(tacs)
            def printTacs(tacs):
                if tacs == []:
                    import time
                    time.sleep(5)
                else:
                    return available.printTactic(tacs[0].ident).then(
                        lambda text: printTacs2(text, tacs[1:]))
            return printTacs(tacs)
        print("Available tactics:")
        return available.tactics().then(cont)

async def readloop(reader, server, write_task):
    while not reader.at_eof():
        data = await reader.read(4096)
        await server.write(data)
    write_task.cancel()


async def writeloop(writer, server):
    while True:
        data = await server.read(4096)
        writer.write(data.tobytes())


async def serve_capnp(reader, writer):
    server = capnp.TwoPartyServer(bootstrap=PushReinforceImpl())

    write_task = asyncio.create_task(writeloop(writer, server))
    coroutines = [write_task, readloop(reader, server, write_task)]

    tasks = asyncio.gather(*coroutines, return_exceptions=True)
    await tasks


async def main():
    s = socket.socket(fileno=sys.stdin.fileno())
    reader, writer = await asyncio.open_connection(sock=s)
    await serve_capnp(reader, writer)

def run_main():
    asyncio.run(main())

if __name__ == '__main__':
    asyncio.run(main())
