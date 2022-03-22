From Tactician Require Import Ltac1. Load NNLearner.
Goal forall A: Prop, A->A.
  intro. intro. apply H.
Qed.
Goal forall A B: Prop, (A->B)->(A->B).
  synth.
Qed.
Goal True.
  synth.
Qed.
