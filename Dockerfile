FROM ubuntu:20.04

MAINTAINER Vasily Pestun "pestun@ihes.fr"

RUN apt-get update
RUN DEBIAN_FRONTEND="noninteractive" apt-get -y install tzdata

# system install build tools
RUN apt-get install -y --no-install-recommends \
    ssh \
    git \
    m4 \
    libgmp-dev \
    opam \
    wget \
    ca-certificates \
    rsync

# system install coq-tactician-reinforce system dependencies
RUN apt-get install -y --no-install-recommends \
    strace \
    graphviz \
    cmake \
    capnproto \
    libcapnp-dev \
    pkg-config \
    libev-dev

# switch to userspace

RUN useradd -m bot
WORKDIR /home/bot
USER bot

# user conda install python

RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
  && sh Miniconda3-latest-Linux-x86_64.sh -b -f

RUN $HOME/miniconda3/bin/conda create -n tac python=3.9
RUN $HOME/miniconda3/envs/tac/bin/pip install pycapnp ptpython graphviz

# user opam install ocaml 4.11.2

RUN opam init --disable-sandboxing --bare --yes
RUN opam switch create tac 4.11.2


# user opam install coq-tactician
RUN opam repo  --switch tac add coq-released https://coq.inria.fr/opam/released
RUN opam repo  --switch tac add coq-extra-dev https://coq.inria.fr/opam/extra-dev
RUN opam repo  --switch tac add coq-core-dev https://coq.inria.fr/opam/core-dev
COPY --chown=bot:bot ./coq-tactician-reinforce.opam.locked coq-tactician-reinforce/coq-tactician-reinforce.opam.locked
RUN opam install --switch tac --yes --deps-only coq-tactician-reinforce/coq-tactician-reinforce.opam.locked


# user opam install coq-tactician-reinforce
COPY --chown=bot:bot . coq-tactician-reinforce
RUN opam install --switch tac --yes  coq-tactician-reinforce/coq-tactician-reinforce.opam.locked
RUN cp $(opam var --switch tac prefix)/.opam-switch/build/coq-tactician-reinforce.~dev/config $(opam var --switch tac coq-tactician:etc)/injection-flags

# check coq-tactician-reinforce

WORKDIR coq-tactician-reinforce
RUN echo "Load NNLearner. Graph Ident plus." | opam exec --switch tac coqtop

RUN opam exec --switch tac -- $HOME/miniconda3/envs/tac/bin/python python/fake_reinforcement_client.py
