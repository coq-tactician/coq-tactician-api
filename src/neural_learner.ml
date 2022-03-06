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

  let init_predict rc wc tacs env =
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

    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let init = Request.initialize_init request in
    let tacs = List.fold_left (fun map tac ->
        let tac = Tactic_normalize.tactic_strict tac in
        let args, tac = Tactic_abstract.tactic_abstract tac in
        TacticMap.add
          (tactic_hash (tactic_make tac)) (tac, args) map)
        TacticMap.empty tacs in
    let tac_arr = Request.Initialize.tactics_init init (TacticMap.cardinal tacs) in
    List.iteri (fun i (hash, (_tac, params)) ->
        let arri = Capnp.Array.get tac_arr i in
        Api.Builder.AbstractTactic.ident_set_int_exn arri hash;
        Api.Builder.AbstractTactic.parameters_set_exn arri (List.length params))
      (TacticMap.bindings tacs);

    let graph = Request.Initialize.graph_init init in
    write_graph graph (fun _ -> 0) builder;

    let definitions =
      let f (_, (_, n)) = Stdint.Uint32.of_int n in
      (List.map f @@ Cmap.bindings known_definitions.constants) @
      (List.map f @@ Indmap.bindings known_definitions.inductives) @
      (List.map f @@ Constrmap.bindings known_definitions.constructors) @
      (List.map f @@ ProjMap.bindings known_definitions.projections)
    in
    ignore (Request.Initialize.definitions_set_list init definitions);
    Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
    match Capnp_unix.IO.ReadContext.read_message rc with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.Initialized -> Feedback.msg_notice Pp.(str "initialized received"); tacs
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

  let predict rc wc tacs ps =
    let module Tactic = Api.Reader.Tactic in
    let module ProofState = Api.Builder.ProofState in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.PredictionProtocol.Prediction in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let predict = Request.predict_init request in
    let updater =
      let open Monad_util.WithMonadNotations(CICGraph) in
      let+ root, context_map = gen_proof_state ps in
      snd root, context_map in
    let (definitions, (root, context_map)), builder =
      CICGraph.run_empty ~def_truncate:true Names.Cmap.empty updater in
    let context = Id.Map.bindings context_map in
    let context_range = List.map (fun (_, (_, n)) -> n) context in
    let context_map_inv = Names.Id.Map.fold_left (fun id (_, node) m -> Int.Map.add node id m) context_map Int.Map.empty in
    let graph = Request.Predict.graph_init predict in
    write_graph graph (fun _ -> 0) builder;
    let state = Request.Predict.state_init predict in
    ProofState.root_set_int_exn state root;
    let _ = ProofState.context_set_list state (List.map Stdint.Uint32.of_int context_range) in
    Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
    match Capnp_unix.IO.ReadContext.read_message rc with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.Prediction preds ->
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
        preds
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 4")

  let drain =
    let drainid = ref 0 in
    fun rc wc ->
      let module Request = Api.Builder.PredictionProtocol.Request in
      let module Response = Api.Reader.PredictionProtocol.Response in
      let request = Request.init_root () in
      let id = !drainid in
      Request.synchronize_set_int_exn request id;
      drainid := id + 1;
      Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
      let rec loop () =
        match Capnp_unix.IO.ReadContext.read_message rc with
        | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3")
        | Some response ->
          let response = Response.of_message response in
          match Response.get response with
          | Response.Synchronized id' when Stdint.Uint64.to_int id' = id -> ()
          | _ -> loop () in
      loop ()

  type model =
    { tactics : glob_tactic_expr list
    ; write_context : Unix.file_descr Capnp_unix.IO.WriteContext.t
    ; read_context : Unix.file_descr Capnp_unix.IO.ReadContext.t }

  let empty () =
    let ours, theirs = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let pid = Unix.create_process
        "pytact-server" [| "pytact-server" |] theirs Unix.stdout Unix.stderr in
    Declaremods.append_end_library_hook (fun () ->
        Unix.kill pid Sys.sigkill;
        ignore (Unix.waitpid [] pid));
    Unix.close theirs;
    { tactics = []
    ; write_context = Capnp_unix.IO.create_write_context_for_fd ~compression:`Packing ours
    ; read_context = Capnp_unix.IO.create_read_context_for_fd ~compression:`Packing ours }

  let learn ({ tactics; _ } as db) _origin _outcomes tac =
    let tac = tactic_repr tac in
    let tactics = tac::tactics in
    last_model := tactics;
    { db with tactics }
  let predict { tactics; write_context; read_context } =
    drain read_context write_context;
    let tacs = init_predict read_context write_context tactics (Global.env ()) in
    fun f ->
      if f = [] then IStream.empty else
        let preds = predict read_context write_context tacs (List.hd f).state in
        let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
        IStream.of_list preds
  let evaluate db _ _ = 0., db

end

let () = register_online_learner "Neural Learner" (module NeuralLearner)
