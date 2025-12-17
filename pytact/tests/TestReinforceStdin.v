Definition comp {A} (f g : A -> A) (x: A) := f (g x).
Definition mid {A} (x: A) := x.

Goal forall (A : Prop), A -> A.
  refine (fun (A : Prop) => _).
  apply comp.
  apply mid.
  apply comp.
  apply mid.
  apply mid.
Qed.

Tactician Explore.
