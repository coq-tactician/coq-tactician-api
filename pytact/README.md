# PyTactician

PyTactician is a Python Library for interfacing with the Coq Proof Assistant and its Tactician plugin and
reading associated datasets. The major version number `x` of this library indicates the version of the
communication protocol. Any PyTactician version `x.y` is compatible with the communication protocol `x`.

## Installation

Binary wheels are provided for Linux and MacOS. On those platforms you can install by executing
`pip install pytactician`. If you need to install from source, you need to have Capt'n Proto >= 0.8 installed
on your system. See the main repo [README](https://github.com/coq-tactician/coq-tactician-reinforce#prerequisites)
for more details on prerequisites.

## Usage

The Python software provides both a software library to work with the graph based datasets extracted from Coq and
a number of executables. Available executables are as follows (use the `--help` flag for each executable to learn
about all the options).

- `pytact-check [-h] [--jobs JOBS] [--quick] [--verbose VERBOSE] dir`
   Run sanity checks on a dataset and print some statistics.
- `pytact-visualize [-h] [--port PORT] [--hostname HOSTNAME] [--dev] dataset`:
   Start an interactive server that visualizes a dataset.
- `pytact-server [-h] [--tcp TCP] [--record RECORD_FILE] {graph,text}`
  Example python server capable of communicating with Coq through Tactician's 'synth' tactic
  To learn how to interface Coq and Tactician with this server, see the sections below.
- `pytact-oracle [-h] [--tcp PORT] [--record RECORD_FILE] {graph,text} dataset`
  A tactic prediction server acting as an oracle, retrieving it's information from a dataset
- `pytact-fake-coq [-h] (--tcp TCP_LOCATION | --stdin EXECUTABLE) data`
  A fake Coq client that connects to a prediction server and feeds it a stream of previously recorded messages.
- `pytact-prover`: A dummy example client that interfaces with Coq and Tactician for reinforcement-learning-style
  communication. To learn how to interface Coq and Tactician with this client, see the sections below.

## Documentation

TODO: Point to documentation
