(lang dune 2.9)
(using dune_site 0.1)
(name coq-tactician-reinforce)
(using coq 0.3)
(generate_opam_files false)

(source (github coq-tactician/coq-tactician-reinforce))
(homepage "https://coq-tactician.github.io")
(authors "Lasse Blaauwbroek <lasse@blaauwbroek.eu>")
(maintainers "Lasse Blaauwbroek <lasse@blaauwbroek.eu>")

(package
 (name coq-tactician-reinforce)
 (synopsis "Reinforcement learning for Tactician")
 (description "")
 (depends
  ocaml
  (coq (= 8.11.2))
  (coq-tactician (= 8.11.dev))
  (coq-tactician-dummy (= dev))
  capnp-rpc-unix
  capnp-rpc-lwt
  ppx_deriving
  (dune (>= 2.9))))


