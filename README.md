# Reinforcement learning for Tactician

## Coq plugin installation

The recommended `opam` version is `>= 2.1.0`. Other versions might work as well, but you may have to install some dependencies manually.
```
opam switch create my-switch --empty
opam repo add coq-released https://coq.inria.fr/opam/released
opam repo add coq-extra-dev https://coq.inria.fr/opam/extra-dev
opam repo add coq-core-dev https://coq.inria.fr/opam/core-dev
opam repo add custom-archive https://github.com/LasseBlaauwbroek/custom-archive.git
git clone git@github.com:coq-tactician/coq-tactician-reinforce.git
opam install ./coq-tactician-reinforce/coq-tactician-reinforce.opam.locked --yes
cp $(opam var prefix)/.opam-switch/build/coq-tactician-reinforce.~dev/config $(opam var coq-tactician:etc)/injection-flags
```
If you encounter problems, try installing `opam install conf-libev`.

Optional but recommended additional software: `graphviz` (install through your distribution's package manager)

Notice: the installation depends on ocaml version 4.11.2 that is in conflict with glibc version >= 2.34
and therefore fails on Ubuntu 21.10. 

## Docker

To verify installation in a controlled enviroment we provide Dockerfile script. To build the docker image, run from the directory containing the Dockerfile
```
docker build -t tac:test . 
```

## Available Commands

These commands will create a graph of some object, and write it to `graph.pdf` (if `graphviz` is available).

The following commands are always available:
```
[Full] [LGraph | Graph | DAG] Ident identifier.
[Full] [LGraph | Graph | DAG] Term term.
```
Normally, the commands print a non-transitive graph. The `[Full]` modifier changes this so that the full transitive graph of definitions is added.

Additionally, in proof mode, these commands are available:
```
[Full] [LGraph | Graph | DAG] Proof.
```

## Reinforcement learning

Finally, the command `Reinforce.` will initiate a reinforcement session. An example of this is available in
[theories/ReinforceTest.v](theories/ReinforceTest.v).
To do this, you need to have a python client running. An example is available in [python/fake_reinforcement_client.py](python/fake_reinforcement_client.py).
You run it from the root of the repository as `python3 python/fake_reinforcement_client.py`.
Everybody is invited to make this a proper python package. Until then, to run the python code, you need the following packages:
```
pip intall graphviz
pip install pycapnp
pip install ptpython
```

If you run
[python/fake_reinforcement_client.py --interactive](python/fake_reinforcement_client.py) then an innteractive shell appears where you can
manually interact with the environment. Whenever a tactic is executed,
the resulting proof state if visualized in the file
`python_graph.pdf`.