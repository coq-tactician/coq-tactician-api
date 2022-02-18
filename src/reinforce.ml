open Tactician_ltac1_record_plugin
open Labelled_graph_capnp_generator
open Names
open Ltac_plugin

module Api = Labelled_graph_api.MakeRPC(Capnp_rpc_lwt)
open Capnp_rpc_lwt
open Lwt.Infix

module G = GlobalGraph
open GB

module TacticMap = Int.Map

exception NoSuchTactic
exception MismatchedArguments
exception IllegalArgument
exception ParseError

let gen_proof_state (hyps : (Constr.t, Constr.t) Context.Named.pt) (concl : Constr.t) =
  let open CICGraph in
  let open Monad_util.WithMonadNotations(CICGraph) in
  let* hyps, (concl, map) = with_named_context hyps @@
    let* map = lookup_named_map in
    let+ concl = gen_constr concl in
    concl, map in
  let+ root = mk_node Root ((ContextSubject, concl)::hyps) in
  root, map

let write_execution_result res hyps concl obj =
  let module ExecutionResult = Api.Builder.ExecutionResult in
  let module Graph = Api.Builder.Graph in
  let module ProofState = Api.Builder.ProofState in

  (* Obtain the graph *)
  let updater =
    let open Monad_util.WithMonadNotations(CICGraph) in
    let open Monad.Make(CICGraph) in
    let+ root, context_map = gen_proof_state hyps concl in
    snd root, context_map in
  let (definitions, (root, context_map)), nodes, paths =
      CICGraph.run_empty ~follow_defs:true Names.Cmap.empty updater in
  let context = Id.Map.bindings context_map in
  let context_range = OList.map (fun (_, (_, n)) -> n) context in
  let context_map_inv = Names.Id.Map.fold_left (fun id (_, node) m -> Int.Map.add node id m) context_map Int.Map.empty in

  (* Write graph to capnp structure *)
  let new_state = ExecutionResult.new_state_init res in
  let capnp_graph = ExecutionResult.NewState.graph_init new_state in
  write_graph capnp_graph (fun _ -> 0) nodes;
  let state = ExecutionResult.NewState.state_init new_state in
  ProofState.root_set_int_exn state root;
  let _ = ProofState.context_set_list state (List.map Stdint.Uint32.of_int context_range) in
  let capability = obj context_map_inv in
  ExecutionResult.NewState.obj_set new_state (Some capability);
  Capability.dec_ref capability

let write_execution_result res obj =
  let open Proofview in
  let open Notations in
  let complete =
    let module ExecutionResult = Api.Builder.ExecutionResult in
    tclUNIT () >>= fun () ->
    ExecutionResult.complete_set res; tclUNIT () in
  tclFOCUS ~nosuchgoal:complete 1 1 @@
  (Tactician_util.pr_proof_tac () >>= fun () ->
   Goal.enter_one (fun gl ->
       let hyps = Goal.hyps gl in
       let concl = Goal.concl gl in
       let sigma = Proofview.Goal.sigma gl in
       let hyps = OList.map (map_named (EConstr.to_constr sigma)) hyps in
       let concl = EConstr.to_constr sigma concl in
       write_execution_result res hyps concl obj; tclUNIT ()))

let write_execution_result state res obj =
  ignore (Pfedit.solve Goal_select.SelectAll None (write_execution_result res obj) state)

let find_tactic tacs id =
  match TacticMap.find_opt id tacs with
  | None -> raise NoSuchTactic
  | Some x -> x

let find_argument context id =
  match Int.Map.find_opt id context with
  | None -> raise MismatchedArguments
  | Some x -> x

let pp_tac tac = Sexpr.format_oneline (Pptactic.pr_glob_tactic (Global.env ()) tac)

let rec proof_object state tacs context_map =
  let module ProofObject = Api.Service.ProofObject in
  let module ExecutionResult = Api.Builder.ExecutionResult in
  let module Exception = Api.Builder.Exception in
  let module Tactic = Api.Reader.Tactic in
  ProofObject.local @@ object
    inherit ProofObject.service

    method run_tactic_impl params release_param_caps =
      release_param_caps ();
      let open ProofObject.RunTactic in
      let response, results = Service.Response.create Results.init_pointer in
      let res = Results.result_init results in
      let tac = Params.tactic_get params in
      let tac_id = Tactic.ident_get_int_exn tac in
      let tac_args = Tactic.arguments_get_list tac in
      begin
        try
          let tac_args = List.map (fun arg ->
              match Tactic.Argument.get arg with
              | Tactic.Argument.Undefined _ | Tactic.Argument.Unresolvable -> raise IllegalArgument
              | Tactic.Argument.Term t -> t
            ) tac_args in
          let tac_args = List.map (fun a -> Stdint.Uint32.to_int (Tactic.Argument.Term.node_index_get a)) tac_args in
          let tac, params = find_tactic tacs tac_id in
          if List.length params <> List.length tac_args then raise MismatchedArguments;
          let tac_args = List.map (find_argument context_map) tac_args in
          let subst = List.combine (List.map snd params) tac_args in
          let tac = Tactic_substitute.tactic_substitute (fun id -> List.assoc id subst) tac in

          let prtac = pp_tac tac in
          Feedback.msg_notice @@ Pp.(str "run tactic " ++ prtac);

          let tac = Ltacrecord.parse_tac tac in
          let nosuchgoal = Proofview.tclZERO (Proof_bullet.SuggestNoSuchGoals (1, state)) in
          let tac = Proofview.tclFOCUS ~nosuchgoal 1 1 tac in
          try
            let state', _safe = Pfedit.solve Goal_select.SelectAll None tac state in
            write_execution_result state' res (proof_object state' tacs)
          with Logic_monad.TacticFailure e ->
            ExecutionResult.failure_set res
        with
        | NoSuchTactic ->
          let exc = ExecutionResult.protocol_error_init res in
          Exception.no_such_tactic_set exc
        | MismatchedArguments ->
          let exc = ExecutionResult.protocol_error_init res in
          Exception.mismatched_arguments_set exc
        | IllegalArgument ->
          let exc = ExecutionResult.protocol_error_init res in
          Exception.illegal_argument_set exc
      end;
      Service.return response
  end

let service_name = Capnp_rpc_net.Restorer.Id.public ""

let available_tactics tacs =
  let module AvailableTactics = Api.Service.AvailableTactics in
  AvailableTactics.local @@ object
    inherit AvailableTactics.service

    method tactics_impl params release_param_caps =
      let open AvailableTactics.Tactics in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let tac_arr = Results.tactics_init results (TacticMap.cardinal tacs) in
      List.iteri (fun i (hash, (_tac, params)) ->
          let arri = Capnp.Array.get tac_arr i in
          Api.Builder.AbstractTactic.ident_set_int_exn arri hash;
          Api.Builder.AbstractTactic.parameters_set_exn arri (List.length params))
        (TacticMap.bindings tacs);
      Service.return response

    method print_tactic_impl params release_param_caps =
      let open AvailableTactics.PrintTactic in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let id = Params.tactic_get_int_exn params in
      let tac, params = find_tactic tacs id in
      let str =
        try Pp.string_of_ppcmds @@ pp_tac tac
        with NoSuchTactic -> "NoSuchTactic" in
      Results.tactic_set results str;
      Service.return response
  end

let pull_reinforce =
  let module Reinforce = Api.Service.PullReinforce in
  Reinforce.local @@ object
    inherit Reinforce.service

    method reinforce_impl params release_param_caps =
      let open Reinforce.Reinforce in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in

      let learner = Tactic_learner_internal.learner_get () in
      let preds = learner.predict [] in
      let preds = List.map (fun p -> Tactic_learner_internal.TS.(p.tactic)) @@ IStream.to_list preds in
      let map = List.fold_left (fun map tac ->
          let open Tactic_learner_internal.TS in
          let tac = tactic_repr tac in
          let tac = Tactic_normalize.tactic_strict tac in
          let args, tac = Tactic_abstract.tactic_abstract tac in
          TacticMap.add
            (tactic_hash (tactic_make tac)) (tac, args) map)
          TacticMap.empty preds in
      let capability = available_tactics map in
      Results.available_set results (Some capability);
      Capability.dec_ref capability;

      let res = Results.result_init results in
      begin try
          let lemm_str = Params.lemma_get params in
          let evd, c = try
              let lemm_constr_expr = Pcoq.parse_string Pcoq.Constr.lconstr lemm_str in
              Constrintern.interp_constr_evars (Global.env ()) Evd.empty lemm_constr_expr
            with e when CErrors.noncritical e ->
              raise ParseError in

          let start = Proof.start ~name:(Names.Id.of_string "dummy") ~poly:false evd [Global.env (), c] in
          write_execution_result start res (proof_object start map)
        with ParseError ->
          let exc = Api.Builder.ExecutionResult.protocol_error_init res in
          Api.Builder.Exception.parse_error_set exc
      end;
      Service.return response
  end

let capnp_main conn =
  let module Main = Api.Service.Main in
  Main.local @@ object
    inherit Main.service

    method initialize_impl params release_param_caps =
      let open Main.Initialize in
      let response, results = Service.Response.create Results.init_pointer in
      let x = Params.push_get params in
      (* let callback s = *)
      (*   let open Api.Client.PushReinforce.Reinforce in *)
      (*   let request, params = Capability.Request.create Params.init_pointer in *)
      (*   let res = Params.result_init params in *)
      (*   execution_result res; *)
      (*   Capability.call_for_unit_exn s method_id request in *)
      Service.return_lwt @@ fun () -> Capability.with_ref (Option.get x) @@ fun s ->
      (* callback s *) Lwt.return_unit >>= fun () ->
      Results.pull_set results (Some pull_reinforce);
      release_param_caps ();
      Lwt.return @@ Ok response
  end


let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let reinforce () =
  Lwt_main.run @@
  (Lwt_switch.with_switch @@ fun switch ->
   let endpoint = Capnp_rpc_unix.Unix_flow.connect (Lwt_unix.of_unix_file_descr Unix.stdin)
                  |> Capnp_rpc_net.Endpoint.of_flow (module Capnp_rpc_unix.Unix_flow)
                    ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
                    ~switch in
   let restore = Capnp_rpc_net.Restorer.single service_name (capnp_main endpoint) in
   let _ : Capnp_rpc_unix.CapTP.t = Capnp_rpc_unix.CapTP.connect ~restore endpoint in
   let w, f = Lwt.wait () in
   Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return @@ Lwt.wakeup f ());
   w);
  Gc.full_major ()
