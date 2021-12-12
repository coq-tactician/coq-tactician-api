FROM pestun/debian.10.opam.2.1.2.ocaml.4.13.1.coq.8.11.dev.conda.python.3.9.capnproto:latest

MAINTAINER Vasily Pestun "pestun@ihes.fr"

RUN eval $(opam env) \
    && opam repo add coq-extra-dev https://coq.inria.fr/opam/extra-dev

COPY --chown=coq:coq . coq-tactician-reinforce

WORKDIR coq-tactician-reinforce

RUN eval $(opam env) \
    && opam install ./coq-tactician-reinforce.opam -y

RUN pip install .

RUN eval $(opam env) \
    && pytact-test
