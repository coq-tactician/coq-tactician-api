FROM ubuntu:20.04

MAINTAINER Vasily Pestun "pestun@ihes.fr"

RUN apt-get update
RUN DEBIAN_FRONTEND="noninteractive" apt-get -y install tzdata
RUN apt-get install -y --no-install-recommends \
    ssh \
    git \
    m4 \
    libgmp-dev \
    opam \
    wget \
    ca-certificates \
    rsync \
    strace \
    graphviz \
    cmake \
    capnproto \
    libcapnp-dev \
    pkg-config \
    libev-dev  

RUN useradd -m bot
WORKDIR /home/bot
USER bot

RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
  && sh Miniconda3-latest-Linux-x86_64.sh -b -f


RUN opam init --disable-sandboxing --bare --yes
RUN opam switch create tac 4.11.2
RUN opam repo  --switch tac add coq-released https://coq.inria.fr/opam/released  
RUN opam repo  --switch tac add coq-extra-dev https://coq.inria.fr/opam/extra-dev 
RUN opam repo  --switch tac add coq-core-dev https://coq.inria.fr/opam/core-dev  


COPY coq-tactician-reinforce/coq-tactician-reinforce.opam.locked coq-tactician-reinforce/coq-tactician-reinforce.opam.locked
RUN opam install --switch tac --yes --deps-only coq-tactician-reinforce/coq-tactician-reinforce.opam.locked


ENV PATH="/home/bot/miniconda3/bin:${PATH}"
RUN conda create -n tac python=3.9 
RUN /home/bot/miniconda3/envs/tac/bin/pip install pycapnp ptpython graphviz


COPY --chown=bot:bot coq-tactician-reinforce temp/coq-tactician-reinforce 
RUN opam install --switch tac --yes  temp/coq-tactician-reinforce/coq-tactician-reinforce.opam.locked
WORKDIR temp/coq-tactician-reinforce


RUN echo "Load NNLearner. Graph Ident plus." | opam exec --switch tac coqtop

RUN cp $(opam var --switch tac prefix)/.opam-switch/build/coq-tactician-reinforce.~dev/config $(opam var --switch tac coq-tactician:etc)/injection-flags

