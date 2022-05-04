(* Set Tactician Reinforce1 Debug. *)
From Tactician Require Import Ltac1. Load NNLearner.
Set Tactician Neural Server "127.0.0.1:33333".
Goal forall A: Prop, A->A.
  intro. intro. apply H.
Qed.
Goal forall A B C D E: Prop, A->B->C->D->E->D.
  synth.
Qed.
