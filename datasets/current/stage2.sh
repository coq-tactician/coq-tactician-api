eval $(opam env --root=./opam-root --switch=default --set-root)
time opam install --yes --keep-build-dir coq-tactician-stdlib.8.11.dev --fake
opam switch export opam-stage2 --freeze
