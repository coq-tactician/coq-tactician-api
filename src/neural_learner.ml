open Ltac_plugin
open Tactician_ltac1_record_plugin
open Tactic_learner
open Names
open Graph_capnp_generator
open Graph_extractor
open Tacexpr

let service_name = Capnp_rpc_net.Restorer.Id.public ""

let last_model = Summary.ref ~name:"neural-learner-lastmodel" []

module NeuralLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

  module Api = Graph_api.MakeRPC(Capnp_rpc_lwt)
  open Capnp_rpc_lwt
  open Lwt.Infix

  (* module G = GlobalGraph *)
  (* module GB = DAGBuilder(G) *)
  (* open GB *)

  let gen_proof_state ps =
    let open GB in
    let hyps = proof_state_hypotheses ps in
    let concl = proof_state_goal ps in
    let hyps = List.map (map_named term_repr) hyps in
    let concl = term_repr concl in
    let open CICGraph in
    let open Monad_util.WithMonadNotations(CICGraph) in
    let* hyps, (concl, map) = with_named_context hyps @@
      let* map = lookup_named_map in
      let+ concl = gen_constr concl in
      concl, map in
    let+ root = mk_node Root ((ContextSubject, concl)::hyps) in
    root, map

  let request_chan = Event.new_channel ()
  let response_chan = Event.new_channel ()

  exception NoSuchTactic
  exception MismatchedArguments
  exception IllegalArgument
  (* exception ParseError *)

  module TacticMap = Int.Map
  let find_tactic tacs id =
    match TacticMap.find_opt id tacs with
    | None -> raise NoSuchTactic
    | Some x -> x

  let find_argument context id =
    match Int.Map.find_opt id context with
    | None -> raise MismatchedArguments
    | Some x -> x

  let pp_tac tac = Sexpr.format_oneline (Pptactic.pr_glob_tactic (Global.env ()) tac)

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

  let rec predict tacs t =
    let module Tactic = Api.Reader.Tactic in
    let module ProofState = Api.Builder.ProofState in
    let module Prediction = Api.Reader.PredictionContext.Prediction in
    let open Api.Client.PredictionContext.Predict in
    let e = Event.receive request_chan in
    match Event.sync e with
    | `Predict ps ->
      let request, params = Capability.Request.create Params.init_pointer in
      let updater =
        let open Monad_util.WithMonadNotations(CICGraph) in
        let+ root, context_map = gen_proof_state ps in
        snd root, context_map in
      let (definitions, (root, context_map)), builder =
        CICGraph.run_empty ~def_truncate:true Names.Cmap.empty updater in
      let context = Id.Map.bindings context_map in
      let context_range = List.map (fun (_, (_, n)) -> n) context in
      let context_map_inv = Names.Id.Map.fold_left (fun id (_, node) m -> Int.Map.add node id m) context_map Int.Map.empty in
      let graph = Params.graph_init params in
      write_graph graph (fun _ -> 0) builder;
      let state = Params.state_init params in
      ProofState.root_set_int_exn state root;
      let _ = ProofState.context_set_list state (List.map Stdint.Uint32.of_int context_range) in
      Capability.call_for_value_exn t method_id request >>= fun res ->
      let preds = Results.predictions_get res in
      let convert_args args =
        let args = List.map (fun arg ->
          match Tactic.Argument.get arg with
          | Tactic.Argument.Undefined _ | Tactic.Argument.Unresolvable -> raise IllegalArgument
          | Tactic.Argument.Term t -> t
        ) args in
        List.map (fun a -> Stdint.Uint32.to_int (Tactic.Argument.Term.node_index_get a)) args in
      let preds = Capnp.Array.map_list preds ~f:(fun p ->
          let tac = Prediction.tactic_get p in
          let tid = Tactic.ident_get_int_exn tac in
          let args = convert_args @@ Tactic.arguments_get_list tac in
          let tac, params = find_tactic tacs tid in
          if List.length params <> List.length args then raise MismatchedArguments;
          let args = List.map (find_argument context_map_inv) args in
          let subst = List.combine (List.map snd params) args in
          let tac = Tactic_substitute.tactic_substitute (fun id -> List.assoc id subst) tac in
          let conf = Prediction.confidence_get p in
          tac, conf
        ) in
      let e = Event.send response_chan preds in
      Event.sync e;
      predict tacs t
    | `Init (tacs, env) ->
      Lwt.return (tacs, env)

  let rec predict_init t tacs env =
    let builder, known_definitions =
      let globrefs = Environ.Globals.view Environ.(env.env_globals) in
      let constants = Cset_env.elements @@ Cmap_env.domain @@ globrefs.constants in
      (* We are only interested in canonical constants *)
      let _constants = Cset.elements @@ List.fold_left (fun m c ->
          let c = Constant.make1 @@ Constant.canonical c in
          Cset.add c m) Cset.empty constants in
      let minductives = Mindmap_env.Set.elements @@ Mindmap_env.domain globrefs.inductives in
      (* We are only interested in canonical inductives *)
      let minductives = Mindset.elements @@ List.fold_left (fun m c ->
          let c = MutInd.make1 @@ MutInd.canonical c in
          Mindset.add c m) Mindset.empty minductives in
      let open Monad_util.WithMonadNotations(CICGraph) in
      let open Monad.Make(CICGraph) in

      let open GB in
      let updater =
        let* () = List.iter (fun c ->
            let+ _ = gen_const c in ()) constants in
        List.iter gen_mutinductive_helper minductives in
      let (known_definitions, ()), builder =
        CICGraph.run_empty ~def_truncate:true Cmap.empty updater in
      builder, known_definitions in

    let open Api.Client.PushReinforce.PredictionContext in
    let request, params = Capability.Request.create Params.init_pointer in
    let map = List.fold_left (fun map tac ->
        let tac = Tactic_normalize.tactic_strict tac in
        let args, tac = Tactic_abstract.tactic_abstract tac in
        TacticMap.add
          (tactic_hash (tactic_make tac)) (tac, args) map)
        TacticMap.empty tacs in
    let capability = available_tactics map in
    Params.available_set params @@ Some capability;

    let graph = Params.graph_init params in
    write_graph graph (fun _ -> 0) builder;

    let definitions =
      let f (_, (_, n)) = Stdint.Uint32.of_int n in
      (List.map f @@ Cmap.bindings known_definitions.constants) @
      (List.map f @@ Indmap.bindings known_definitions.inductives) @
      (List.map f @@ Constrmap.bindings known_definitions.constructors) @
      (List.map f @@ ProjMap.bindings known_definitions.projections)
    in
    ignore (Params.definitions_set_list params definitions);

    Capability.call_and_wait t method_id request >>= function
    | Ok (res, release) ->
      let predictor = Option.get @@ Results.result_get res in
      Capability.with_ref predictor (predict map) >>= fun (init, env) ->
      release ();
      predict_init t init env
    (* let emb = Results.emb_get res in *)
    | Error _ -> CErrors.anomaly Pp.(str "Capnp protocol error")

  let prediction_loop service =
    let e = Event.receive request_chan in
    match Event.sync e with
    | `Predict _ -> CErrors.anomaly Pp.(str "Coq <-> Lwt protocol error")
    | `Init (tacs, env) ->
      predict_init service tacs env

  let () =
    let ours, theirs = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    ignore(Unix.create_process
             "pytact-server" [| "pytact-server" |] theirs Unix.stdout Unix.stderr);
    let predictor () =
      Lwt_main.run @@
      Lwt_switch.with_switch @@ fun switch ->
       let endpoint = Capnp_rpc_unix.Unix_flow.connect (Lwt_unix.of_unix_file_descr ours)
                      |> Capnp_rpc_net.Endpoint.of_flow (module Capnp_rpc_unix.Unix_flow)
                        ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
                        ~switch in
       let conn = Capnp_rpc_unix.CapTP.connect ~restore:Capnp_rpc_net.Restorer.none endpoint in
       let main = Capnp_rpc_unix.CapTP.bootstrap conn service_name in
       Capability.with_ref main prediction_loop in
    ignore(Thread.create predictor ())

  let init_predict tacs env =
    let e = Event.send request_chan (`Init (tacs, env)) in
    Event.sync e

  let predict ps =
    let e = Event.send request_chan (`Predict ps) in
    Event.sync e;
    let e = Event.receive response_chan in
    Event.sync e

  type model = glob_tactic_expr list

  let empty () = []
  let learn db _origin _outcomes tac =
    let tac = tactic_repr tac in
    let db = tac::db in
    last_model := db;
    db
  let predict db =
    init_predict db (Global.env ());
    fun f ->
      if f = [] then IStream.empty else
        let preds = predict (List.hd f).state in
        let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
        IStream.of_list preds
  let evaluate db _ _ = 0., db

end

let () = register_online_learner "Neural Learner" (module NeuralLearner)
