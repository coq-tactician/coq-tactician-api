#!/usr/bin/env python3
import os
import sys
import asyncio
import socket
from pathlib import Path

# Load the cap'n proto library, and the communication specification in 'graph_api.capnp'
import capnp
capnp.remove_import_hook()
graph_api_capnp = str(Path('graph_api.capnp').expanduser())
graph_api_capnp = capnp.load(graph_api_capnp)

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
class PushReinforceImpl(graph_api_capnp.PushReinforce.Server):

    # This function is tricky, because you cannot use asyncio in it. You have to use
    # the C++ async library, which uses 'then()' as a chaining mechanism. This may cause
    # some poblems.
    def reinforce(self, result, _context):
        print('Fake Reinforcer: reinforce')
        print(result)
        def p(r):
            print(r)
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

async def main():

    # Create a socket pair, initialize cap'n proto on our end of the socket
    rsock, wsock = socket.socketpair()
    reader, writer = await asyncio.open_connection(sock=rsock)
    client = capnp.TwoPartyClient()
    write_task = asyncio.create_task(write_loop(client, writer))
    coroutines = [read_loop(client, reader, write_task), write_task]
    tasks = asyncio.gather(*coroutines, return_exceptions=True)
    main = client.bootstrap().cast_as(graph_api_capnp.Main)

    # Start Coq, giving the other end of the socket as stdin, and sending stdout to our stdout
    proc = await asyncio.create_subprocess_exec(
        'python3', 'python/fake_coq_server.py',
        # 'coqc', 'Test2.v',
        stdin=wsock,
        stdout=None,
        stderr=None)

    # Initialize the connection. This sends an object to Coq, that it can call to initiate
    # a reinforcement session, or cache an embedding. In this way, python also acts a a server,
    # even though the connection is initiated as if python is the client.
    computation = await main.initialize(PushReinforceImpl()).a_wait()

    # Here, we initiate a reinforcement session from python's side. This is reasonably nice,
    # because you can wrap the code into asyncio using 'a_wait'. It is still slow though.
    # Bigger problems occur in 'PushReinforceImpl', were we cannot wrap in asyncio.
    pull = computation.pull
    resp = await pull.reinforce("forall A : Prop, A -> A").a_wait()
    print(resp.result);
    print(resp.result.which());
    state = resp.result.newState
    obj = state.obj
    obj2 = await obj.runTactic({ 'id': 5, 'arguments': []}).a_wait()
    print(obj2.result)

    # Closing the writer will cause Coq to end the session
    writer.close()
    await proc.communicate()
    await tasks

if __name__ == '__main__':
    asyncio.run(main())
