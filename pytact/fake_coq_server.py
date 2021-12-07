"""
TODO: check if works, fix or deprecate

This file may be useful to Vasily/Fidel/Mirek/Jason when you want
to test a reinforcement client without having to install the Opam
packages. Wasn't maintained for a while
"""


import os
import sys

import socket
import asyncio

from pathlib import Path

import capnp
capnp.remove_import_hook()

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

class ProofObjectImpl(graph_api_capnp.ProofObject.Server):

    def runTactic(self, tactic, _context):
        print("Fake Coq: run tactic")
        print(tactic)
        return {
            'newState': {
                'graph': {
                    'classifications': [],
                    'edges': []
                },
                'state': {
                    'root': 2,
                    'context': []
                },
                'obj': ProofObjectImpl()
            }
        }

class PullReinforceImpl(graph_api_capnp.PullReinforce.Server):

    def reinforce(self, lemma, _context):
        print('Fake Coq: start')
        print(lemma)
        return {
            'newState': {
                'graph': {
                    'classifications': [],
                    'edges': []
                },
                'state': {
                    'root': 2,
                    'context': []
                },
                'obj': ProofObjectImpl()
            }
        }

class MainImpl(graph_api_capnp.Main.Server):

    def initialize(self, push, _context):
        print('Fake Coq: initialize')
        print(push)
        state = {
            'newState': {
                'graph': {
                    'classifications': [],
                    'edges': []
                },
                'state': {
                    'root': 2,
                    'context': []
                },
                'obj': ProofObjectImpl()
            }
        }
        return push.reinforce(state).then(
            lambda _: setattr(_context.results, "pull", PullReinforceImpl()))

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
    server = capnp.TwoPartyServer(bootstrap=MainImpl())

    write_task = asyncio.create_task(writeloop(writer, server))
    coroutines = [write_task, readloop(reader, server, write_task)]

    tasks = asyncio.gather(*coroutines, return_exceptions=True)
    await tasks


async def main():
    s = socket.socket(fileno=sys.stdin.fileno())
    reader, writer = await asyncio.open_connection(sock=s)
    await serve_capnp(reader, writer)


if __name__ == '__main__':
    asyncio.run(main())
