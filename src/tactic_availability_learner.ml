open Tactician_ltac1_record_plugin
open Tactic_learner

module TacticAvailabilityLearner = functor (TS : TacticianStructures) -> struct
  open TS

  type model = tactic list

  let empty () = []

  let learn db _status _loc _outcomes tac = tac::db

  let predict db f =
    let out = List.map (fun tactic -> { confidence = 0.; focus = 0; tactic }) db in
    IStream.of_list out

  let evaluate db _ _ = 1., db

end

let () = register_online_learner "tactic-availability-learner" (module TacticAvailabilityLearner)
