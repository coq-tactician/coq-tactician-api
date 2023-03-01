eval $(opam env --root=./opam-root --switch=default --set-root)
time opam install --keep-build-dir --assume-depext --yes coq-aac-tactics.8.11.0 coq-alea.8.11.0 coq-almost-full.8.14.0 coq-antivalence.1.0.1 coq-atbr.8.11.0 coq-autosubst.1.7 coq-bbv.1.3 coq-bertrand.8.12.0 coq-bignums.8.11.0 coq-bits.1.1.0 coq-buchberger.8.11.0 coq-bytestring.0.9.0 coq-ceres.0.4.0 coq-chapar.8.13.0 coq-chick-blog.1.0.1 coq-color.1.8.2 coq-compcert coq-coq2html.1.3 coq-coqeal.1.1.0 coq-coqoban.8.13.0 coq-coqprime.1.0.5 coq-coqprime-generator.1.1.1 coq-coqtail.8.14 coq-coquelicot.3.2.0 coq-coqutil.0.0.2 coq-corn.8.16.0 coq-cunit.1.0.0 coq-deriving.0.1.0 coq-dijkstra.0.1.0 coq-dpdgraph.0.6.7 coq-equations.1.2.4+8.11 coq-error-handlers.1.2.0 coq-ext-lib.0.11.7 coq-extructures.0.3.1 coq-flocq.3.4.3 coq-flocq-quickchick.1.0.2 coq-fourcolor.1.2.5 coq-function-ninjas.1.0.0 coq-functional-algebra.1.0.2 coq-gaia-numbers.1.15 coq-gaia-ordinals.1.15 coq-gaia-schutte.1.15 coq-gaia-stern.1.15 coq-gaia-theory-of-sets.1.15 coq-gappa.1.5.3 coq-generic-environments.8.11.0 coq-geometric-algebra.0.8.11 coq-giskard.1.0 coq-goedel.8.13.0 coq-hammer.1.3.2+8.11 coq-hammer-tactics.1.3.2+8.11 coq-haskell.1.0.0 coq-high-school-geometry.8.12.0 coq-hoare-tut.8.11.1 coq-hott.8.11 coq-huffman.8.12.0 coq-hydra-battles.0.4 coq-interval.4.6.1 coq-io.4.0.0 coq-io-hello-world.1.2.0 coq-io-system.2.4.1 coq-io-system-ocaml.2.3.1 coq-iris.3.4.0 coq-iris-heap-lang.3.4.0 coq-iris-string-ident.0.1.0 coq-iterable.1.0.0 coq-itree.4.0.0 coq-itree-io.0.1.0 coq-jmlcoq.8.15.0 coq-jsast.2.0.0 coq-libhyps.2.0.6 coq-list-plus.1.1.0 coq-list-string.2.1.2 coq-ltac-iter.1.1.2 coq-math-classes.8.15.0 coq-mathcomp-abel.1.2.1 coq-mathcomp-algebra.1.14.0 coq-mathcomp-apery.1.0.1 coq-mathcomp-bigenough.1.0.1 coq-mathcomp-character.1.14.0 coq-mathcomp-field.1.14.0 coq-mathcomp-fingroup.1.14.0 coq-mathcomp-finmap.1.5.1 coq-mathcomp-multinomials.1.5.5 coq-mathcomp-odd-order.1.14.0 coq-mathcomp-real-closed.1.1.2 coq-mathcomp-solvable.1.14.0 coq-mathcomp-ssreflect.1.14.0 coq-menhirlib.20190924 coq-metacoq.1.0~beta2+8.11 coq-metacoq-erasure.1.0~beta2+8.11 coq-metacoq-pcuic.1.0~beta2+8.11 coq-metacoq-safechecker.1.0~beta2+8.11 coq-metacoq-template.1.0~beta2+8.11 coq-metacoq-translations.1.0~beta2+8.11 coq-mi-cho-coq.1.0.0 coq-min-imports.1.0.2 coq-moment.1.2.1 coq-ott.0.33 coq-paco.4.1.1 coq-paramcoq.1.1.3+coq8.11 coq-pi-agm.1.2.5 coq-pocklington.8.12.0 coq-poltac.0.8.11 coq-printf.2.0.0 coq-qarith-stern-brocot.8.12.0 coq-quickchick.1.6.4 coq-record-update.0.3.1 coq-reduction-effects.0.1.4 coq-regexp-brzozowski.1.0 coq-reglang.1.1.3 coq-relation-algebra.1.7.4 coq-rewriter.0.0.2 coq-rsa.8.8.0 coq-semantics.8.14.0 coq-simple-io.1.8.0 coq-smpl.8.11 coq-smtcoq.2.0+8.11 coq-stalmarck.8.11.0 coq-tlc.20200328 coq-topology.10.0.1 coq-type-infer.0.1.0 coq-zorns-lemma.10.0.1 coq-vst coq-qcert --fake
opam switch export opam-stage3 --freeze
