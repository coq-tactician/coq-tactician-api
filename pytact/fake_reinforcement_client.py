import os
import sys
import asyncio
import socket
import argparse
from pathlib import Path
import pytact.graph_visualize as gv
import pytact.common

# Load the cap'n proto library, and the communication specification in 'graph_api.capnp'
import capnp
capnp.remove_import_hook()



labelled_graph_api = capnp.load(pytact.common.labelled_graph_api_filename)

# Boilerplate code needed to have cap'n proto communicate through asyncio
async def read_loop(client, reader, write_task):
    while not reader.at_eof():
        data = await reader.read(4096)
        client.write(data)
    write_task.cancel()

async def write_loop(client, writer):
    while True:
        data = await client.read(4096)
        writer.write(data.tobytes())
        await writer.drain()

# A class that implements reinforcement learning when it is initiated from Coq's side
class PushReinforceImpl(labelled_graph_api.PushReinforce.Server):

    # This function is tricky, because you cannot use asyncio in it. You have to use
    # the C++ async library, which uses 'then()' as a chaining mechanism. This may cause
    # some poblems.
    def reinforce(self, result, _context):
        print('Fake Reinforcer: reinforce')
        print(result)
        def imp(result, x):
            if x == 0:
                return
            else:
                return result.newState.obj.runTactic({ 'id': x, 'arguments': []}).then(
                    lambda p: imp(p.result, x - 1))
        return imp(result, 20)

    def embed(self, graph, root, _context):
        print('Fake Reinforcer: embed')
        print(graph)
        print(root)
        return [1, 2, 3, 4, 5]

# Helper function to visualize an execution result
def visualize(state):
    kind = state.which()
    if kind == 'newState':
        gv.visualize(state.newState.graph, state.newState.state)
    else:
        gv.visualize_exception(state)

# Reference to a local node (with depIndex 0)
def localNode(id):
    return {'depIndex': 0, 'nodeIndex': id}

# Helper function that runs a given tactic on a given proof state
async def runTactic(obj, ident, args):
    resp = await obj.runTactic({ 'ident': ident, 'arguments': args}).a_wait()
    state = resp.result
    visualize(state)
    return state

# Intitiates a new reinforcement learning session with a given lemma
async def reinforce(pull, lemma):
    resp = await pull.reinforce(lemma).a_wait()
    available_cap = resp.available
    available = await available_cap.tactics().a_wait()
    tacs = []
    print("Available tactics:")
    for tac in available.tactics:
        tac_str = await available_cap.printTactic(tac.ident).a_wait()
        tacs.append((tac.ident, tac_str.tactic))
    for ident, s in tacs:
        print("{} : {}".format(ident, s))
    visualize(resp.result)
    return resp.result, tacs

async def main():
    parser = argparse.ArgumentParser(
        description='example of python code interacting with coq-tactician-reinforce',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    
    parser.add_argument('--interactive',
                        action='store_true',
                        help='drop to the python shell after proof execution')

    test_filename = pytact.common.test_filename_stdin

    parser.add_argument('--file', type=str, default=test_filename,
                        help='path to coq source code file (.v extension) to execute in coq-tactician-reinforce')

    args = parser.parse_args()

    # Create a socket pair, initialize cap'n proto on our end of the socket
    rsock, wsock = socket.socketpair()
    reader, writer = await asyncio.open_connection(sock=rsock)
    client = capnp.TwoPartyClient()
    write_task = asyncio.create_task(write_loop(client, writer))
    coroutines = [read_loop(client, reader, write_task), write_task]
    tasks = asyncio.gather(*coroutines, return_exceptions=True)
    main = client.bootstrap().cast_as(labelled_graph_api.Main)

    # Start Coq, giving the other end of the socket as stdin, and sending stdout to our stdout


    proc = await asyncio.create_subprocess_exec(
        # 'python3', 'python/fake_coq_server.py',
        'tactician', 'exec', 'coqc', test_filename,
        stdin=wsock,
        stdout=None,
        stderr=None)

    # Initialize the connection. This sends an object to Coq, that it can call to initiate
    # a reinforcement session, or cache an embedding. In this way, python also acts a a server,
    # even though the connection is initiated as if python is the client.
    initialized = await main.initialize(PushReinforceImpl()).a_wait()

    # Here, we initiate a reinforcement session from python's side. This is reasonably nice,
    # because you can wrap the code into asyncio using 'a_wait'. It is still slow though.
    # Bigger problems occur in 'PushReinforceImpl', where we cannot wrap in asyncio.
    pull = initialized.pull

    state, tacs = await reinforce(pull, "forall A B C : Prop, (A -> B -> C) -> A -> B -> C")
    state = await runTactic(state.newState.obj, 870093143, [])
    state = await runTactic(state.newState.obj, 870093143, [])
    state = await runTactic(state.newState.obj, 870093143, [])
    state = await runTactic(state.newState.obj, 870093143, [])
    state = await runTactic(state.newState.obj, 870093143, [])
    state = await runTactic(state.newState.obj, 870093143, [])
    state = await runTactic(state.newState.obj, 165468576, [localNode(7)])
    state = await runTactic(state.newState.obj, 165468576, [localNode(13)])
    state = await runTactic(state.newState.obj, 165468576, [localNode(15)])

    if args.interactive:
        from ptpython.repl import embed
        await embed(globals(), locals(), return_asyncio_coroutine=True, patch_stdout=True)

    # Closing the writer will cause Coq to end the session
    writer.close()
    await proc.communicate()
    await tasks

def run_main():
    asyncio.run(main())

if __name__ == '__main__':
    run_main()
    
