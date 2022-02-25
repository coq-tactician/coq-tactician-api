FROM pestun/coq_python_reinforce_deps

MAINTAINER Vasily Pestun "pestun@ihes.fr"

COPY --chown=coq:coq . coq-tactician-reinforce

WORKDIR coq-tactician-reinforce

RUN eval $(opam env) && opam update \
    && opam install -t ./coq-tactician-reinforce.opam -y

RUN pip install .

RUN eval $(opam env) \
    && pytact-test
