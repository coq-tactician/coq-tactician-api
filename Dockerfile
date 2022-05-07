FROM pestun/coq_python_reinforce_deps

MAINTAINER Vasily Pestun "pestun@ihes.fr"

COPY --chown=coq:coq . coq-tactician-reinforce

WORKDIR coq-tactician-reinforce

RUN eval $(opam env) && opam update \
    && opam install -t ./coq-tactician-reinforce.opam -y

RUN pip install .


# run script proof as in former pytact-test
RUN eval $(opam env) \
    && pytact-prover --with-coq --loglevel=INFO

# run script proof over a single tcp connection

RUN eval $(opam env) \
    && pytact-prover --tcp --with-coq --tcp-sessions 1  --loglevel=INFO

# run dfs proof on a sample file prop with 4 variables in a single tcp session

RUN eval $(opam env) \
    && pytact-prover  --tcp --with-coq --tcp-sessions 1 --dfs --test prop4.txt --loglevel=ERROR --dfs-limit=20
