# opam init --root=./opam-root --no-setup --bare
# eval $(opam env --root=./opam-root --switch=default --set-root)
opam switch create . --empty --repos=custom-archive=git+https://github.com/LasseBlaauwbroek/custom-archive.git,coq-extra-dev=https://coq.inria.fr/opam/extra-dev,coq-core-dev=https://coq.inria.fr/opam/core-dev,coq-released=https://coq.inria.fr/opam/released,default
eval $(opam env --root=./opam-root --switch=default --set-root)
time opam install --keep-build-dir --yes --assume-depext ./coq-tactician-reinforce/coq-tactician-reinforce.opam lwt.4.5.0 ocaml-base-compiler.4.09.1 --fake
opam switch export opam-stage2 --freeze
