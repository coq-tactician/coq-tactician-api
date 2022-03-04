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

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api = capnp.load(graph_api_capnp)

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

# Helper function to visualize an execution result
def visualize(state):
    kind = state.which()
    if kind == 'newState':
        gv.visualize(state.newState.graph, state.newState.state)
    else:
        gv.visualize_exception(state)

# Reference to a local node (with depIndex 0)
def localNode(id):
    return {'term': {'depIndex': 0, 'nodeIndex': id}}

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
    main = client.bootstrap().cast_as(graph_api.Main)

    # Start Coq, giving the other end of the socket as stdin, and sending stdout to our stdout
    proc = await asyncio.create_subprocess_exec(
        # 'python3', 'python/fake_coq_server.py',
        'tactician', 'exec', 'coqc', test_filename,
        stdin=wsock,
        stdout=None,
        stderr=None)

    # Here, we initiate a reinforcement session from python's side. This is reasonably nice,
    # because you can wrap the code into asyncio using 'a_wait'. It is still slow though.
    # Bigger problems occur in 'PushReinforceImpl', where we cannot wrap in asyncio.

    state, tacs = await reinforce(pull, "forall A B C : Prop, (A -> B -> C) -> A -> B -> C")
    state = await runTactic(state.newState.obj, 126567959, [])
    state = await runTactic(state.newState.obj, 126567959, [])
    state = await runTactic(state.newState.obj, 126567959, [])
    state = await runTactic(state.newState.obj, 126567959, [])
    state = await runTactic(state.newState.obj, 126567959, [])
    state = await runTactic(state.newState.obj, 126567959, [])
    state = await runTactic(state.newState.obj, 165468576, [localNode(11)])
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

