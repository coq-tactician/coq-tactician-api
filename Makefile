SUBMODULE_COMMIT_ID := $(shell sh Makefile_get_commit.sh)
all:  opam build

opam:  coq-tactician-reinforce.opam.pretemplate 
ifdef SUBMODULE_COMMIT_ID
	@echo "populating coq-tactician-reinforce.opam.template with pin-depends for coq-tactician at ${SUBMODULE_COMMIT_ID}"
	@sed s/___SUBMODULE_COMMIT_ID___/"${SUBMODULE_COMMIT_ID}/" coq-tactician-reinforce.opam.pretemplate > coq-tactician-reinforce.opam.template
else
	$(error "SUBMODULE_COMMIT_ID" is not available)
endif

build: opam dune-project
	dune build
