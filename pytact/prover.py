"""
a test of connection to coq-tactician-api
"""

import sys
import asyncio
import socket
import capnp
import pytact.graph_api_capnp as api
import time
import psutil
import os

async def example_script_prover(pull):
    with open("timings.txt", "w") as fp:
        state = await pull.reinforce("forall (A : Prop), A -> A")
        state = await state.result.newState.obj.runTextTactic("refine (fun (A : Prop) => _)")
        state = await state.result.newState.obj.runTextTactic("set (comp (A: Type) (f g: A -> A) (x: A) := f (g x))")
        state = await state.result.newState.obj.runTextTactic("set (mid (A: Type) (x: A) := x)")

        for i in range(200000):
            start = time.perf_counter()
            state = await state.result.newState.obj.runTextTactic("apply comp")
            state = await state.result.newState.obj.runTextTactic("apply mid")
            end = time.perf_counter()
            fp.write(f"{end - start:.6f}\n")
            print(f"{i} Time: {end - start:.6f} seconds")
        state = await state.result.newState.obj.runTextTactic("refine (fun (x : A) => x)")
        assert state.result.which() == 'complete'  # if this is correct proof
        print("done")

async def main():

    print("Python: creating Unix socketpair and giving the other end as stdin to coqc",
            file=sys.stderr)

    py_sock, coq_sock = socket.socketpair()
    print(f"running tactician exec coqc pytact/tests/TestReinforceStdin.v")
    proc = await asyncio.create_subprocess_exec(
        'tactician', 'exec', 'coqc', '--', 'pytact/tests/TestReinforceStdin.v',
        stdin=coq_sock)

    coq_sock.close()

    connection = await capnp.AsyncIoStream.create_connection(sock=py_sock)
    client = capnp.TwoPartyClient(connection)
    cap = client.bootstrap().cast_as(api.PullReinforce)
    await example_script_prover(cap)

if __name__ == '__main__':
    asyncio.run(capnp.run(main()))
