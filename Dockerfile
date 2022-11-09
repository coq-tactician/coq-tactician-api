FROM coqorg/coq:8.11.2-ocaml-4.11.2-flambda

MAINTAINER Vasily Pestun "pestun@ihes.fr"

# conda + pythnon 3.10

RUN curl https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -o \
    Miniconda3-latest-Linux-x86_64.sh && sh Miniconda3-latest-Linux-x86_64.sh -b -f
ENV HOME="/home/coq"
ENV CONDA_EXE="${HOME}/miniconda3/bin/conda"

RUN $CONDA_EXE create -n python3.10 python=3.10 -y
ENV CONDA_PREFIX="${HOME}/miniconda3/envs/python3.10"
ENV CONDA_PYTHON_EXE="${HOME}/miniconda3/bin/python"
RUN echo 'PATH=$CONDA_PREFIX/bin:$PATH' >> .profile
RUN $CONDA_EXE activate python3.10
RUN $CONDA_EXE -c conda-forge capnprot

# apt-get level project dependencies

RUN sudo apt-get update
RUN sudo apt-get --yes install graphviz pkg-config libev-dev libxxhash-dev cmake build-essential

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
