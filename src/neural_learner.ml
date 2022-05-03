open Ltac_plugin
open Tactician_ltac1_record_plugin
open Tactic_learner
open Names
open Graph_capnp_generator
open Graph_extractor
open Graph_def
open Tacexpr


let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h tags k ppf fmt =
      Format.kfprintf k ppf ("%s: %a @[" ^^ fmt ^^ "@]@.")
        (Logs.Src.name src)
        Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }


let src = Logs.Src.create "neural_learner" ~doc:"coq-tactician-reinforce neural_learner events"
module ThisLogs = (val Logs.src_log src : Logs.LOG)


let service_name = Capnp_rpc_net.Restorer.Id.public ""

let declare_bool_option ~name ~default =
  let key = ["Tactician"; "Neural"; name] in
  Goptions.declare_bool_option_and_ref
    ~depr:false ~name:(String.concat " " key)
    ~key ~value:default

let declare_int_option ~name ~default =
  let open Goptions in
  let key = ["Tactician"; "Neural"; name] in
  let r_opt = ref default in
  let optwrite v = r_opt := match v with | None -> default | Some v -> v in
  let optread () = Some !r_opt in
  let _ = declare_int_option {
      optdepr = false;
      optname = String.concat " " key;
      optkey = key;
      optread; optwrite
    } in
  fun () -> !r_opt

let declare_string_option ~name ~default =
  let open Goptions in
  let key = ["Tactician"; "Neural"; name] in
  let r_opt = ref default in
  let optwrite v = r_opt := v in
  let optread () = !r_opt in
  let _ = declare_string_option {
      optdepr = false;
      optname = String.concat " " key;
      optkey = key;
      optread; optwrite
    } in
  optread

let truncate_option = declare_bool_option ~name:"Truncate" ~default:true
let textmode_option = declare_bool_option ~name:"Textmode" ~default:false

let last_model = Summary.ref ~name:"neural-learner-lastmodel" []

type path = Local | Global
module PathSet = Set.Make(struct type t = path let compare = compare end)

module G = GlobalGraph(PathSet)(struct type result = graph_state end)
module CICGraph = struct
  type node' = path * int
  include CICGraphMonad(G)
end
module GB = GraphBuilder(CICGraph)
module CapnpGraphWriter = CapnpGraphWriter(struct type nonrec path = path end)(G)

module NeuralLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

  module Api = Graph_api.MakeRPC(Capnp_rpc_lwt)

  let gen_proof_state env ps =
    let open GB in
    let hyps = proof_state_hypotheses ps in
    let concl = proof_state_goal ps in
    let hyps = List.map (map_named term_repr) hyps in
    let concl = term_repr concl in
    let open CICGraph in
    let open Monad_util.WithMonadNotations(CICGraph) in
    let* hyps, (concl, map) = with_named_context env (Id.Map.empty, Cmap.empty) hyps @@
      let* map = lookup_named_map in
      let+ concl = gen_constr env (Id.Map.empty, Cmap.empty) concl in
      concl, map in
    let+ root = mk_node ProofState ((ContextSubject, concl)::hyps) in
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

  let find_local_argument context_map =
    let context_map_inv =
      Names.Id.Map.fold (fun id (_, node) m -> Int.Map.add node (Tactic_one_variable.TVar id) m)
        context_map Int.Map.empty in
    fun id ->
    match Int.Map.find_opt id context_map_inv with
    | None -> raise MismatchedArguments
    | Some x -> x

  let find_global_argument
      CICGraph.{ section_nodes; definition_nodes = { constants; inductives; constructors; projections }; _ } =
    let open Tactic_one_variable in
    let map = Cmap.fold (fun c (_, (_, node)) m -> Int.Map.add node (TRef (GlobRef.ConstRef c)) m)
        constants Int.Map.empty in
    let map = Indmap.fold (fun c (_, (_, node)) m -> Int.Map.add node (TRef (GlobRef.IndRef c)) m)
        inductives map in
    let map = Constrmap.fold (fun c (_, (_, node)) m -> Int.Map.add node (TRef (GlobRef.ConstructRef c)) m)
        constructors map in
    let map = ProjMap.fold (fun c (_, node) m ->
        Feedback.msg_notice Pp.(str (Graph_def.projection_to_string c));
        (* TODO: At some point we have to deal with this. One possibility is using `Projection.Repr.constant` *)
        assert false)
        projections map in
    let map = Id.Map.fold (fun c (_, node) m -> Int.Map.add node (TRef (GlobRef.VarRef c)) m)
        section_nodes map in
    fun id ->
      match Int.Map.find_opt id map with
      | None -> raise MismatchedArguments
      | Some x -> x

  let init_predict_text rc wc =

    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    ignore(Request.initialize_init request);
    Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
    match Capnp_unix.IO.ReadContext.read_message rc with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.Initialized -> ()
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

  let init_predict rc wc tacs env =
    let builder, state =
      let globrefs = Environ.Globals.view Environ.(env.env_globals) in
      (* We are only interested in canonical constants *)
      let constants = Cset.elements @@ Cmap_env.fold (fun c _ m ->
          let c = Constant.make1 @@ Constant.canonical c in
          Cset.add c m) globrefs.constants Cset.empty in
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
            let+ _ = gen_const env (Id.Map.empty, Cmap.empty) c in ()) constants in
        List.iter (gen_mutinductive_helper env (Id.Map.empty, Cmap.empty)) minductives in
      let ( state, ()), builder =
        CICGraph.run_empty ~def_truncate:(truncate_option ()) updater G.builder_nil Global in
      builder, state in

    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let init = Request.initialize_init request in
    let tacs = List.fold_left (fun map tac ->
        let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac in
        let (args, tactic_exact), interm_tactic = Tactic_one_variable.tactic_one_variable tac in
        let base_tactic = Tactic_one_variable.tactic_strip tac in
        TacticMap.add
          (tactic_hash (tactic_make base_tactic)) (base_tactic, List.length args) map)
        TacticMap.empty tacs in
    let tac_arr = Request.Initialize.tactics_init init (TacticMap.cardinal tacs) in
    List.iteri (fun i (hash, (_tac, params)) ->
        let arri = Capnp.Array.get tac_arr i in
        Api.Builder.AbstractTactic.ident_set_int_exn arri hash;
        Api.Builder.AbstractTactic.parameters_set_exn arri params)
      (TacticMap.bindings tacs);

    let graph = Request.Initialize.graph_init init in
    CapnpGraphWriter.write_graph graph (fun _ -> 0) builder.node_count builder.edge_count builder.builder;

    let definitions =
      let f (_, (_, (_, n))) = Stdint.Uint32.of_int n in
      (List.map f @@ Cmap.bindings state.definition_nodes.constants) @
      (List.map f @@ Indmap.bindings state.definition_nodes.inductives) @
      (List.map f @@ Constrmap.bindings state.definition_nodes.constructors) @
      (List.map f @@ ProjMap.bindings state.definition_nodes.projections) @
      (List.map (fun (_, (_, n)) -> Stdint.Uint32.of_int n) @@ Id.Map.bindings state.section_nodes)
    in
    ignore (Request.Initialize.definitions_set_list init definitions);
    Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
    match Capnp_unix.IO.ReadContext.read_message rc with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.Initialized -> tacs, state
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

  let predict_text rc wc env ps =
    let module Tactic = Api.Reader.Tactic in
    let module Argument = Api.Reader.Argument in
    let module ProofState = Api.Builder.ProofState in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.PredictionProtocol.TextPrediction in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let predict = Request.predict_init request in
    let state = Request.Predict.state_init predict in
    let hyps = List.map (map_named term_repr) @@ proof_state_hypotheses ps in
    let concl = term_repr @@ proof_state_goal ps in
    ProofState.text_set state @@ Graph_extractor.proof_state_to_string_safe (hyps, concl) env Evd.empty;
    Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
    match Capnp_unix.IO.ReadContext.read_message rc with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.TextPrediction preds ->
        let preds = Capnp.Array.to_list preds in
        let preds = List.filter_map (fun p ->
            try
              let tac = Tacintern.intern_pure_tactic (Genintern.empty_glob_sign env) @@
                Pcoq.parse_string Pltac.tactic_eoi @@ Prediction.tactic_text_get p in
              let conf = Prediction.confidence_get p in
              Some (tac, conf)
            with e when CErrors.noncritical e ->
              None
          ) preds in
        preds
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 4")

  let predict rc wc find_global_argument state tacs env ps =
    let module Tactic = Api.Reader.Tactic in
    let module Argument = Api.Reader.Argument in
    let module ProofState = Api.Builder.ProofState in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.PredictionProtocol.Prediction in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let predict = Request.predict_init request in
    let updater =
      let open Monad_util.WithMonadNotations(CICGraph) in
      let+ root, context_map = gen_proof_state env ps in
      snd root, context_map in
    let (_, (root, context_map)), G.{ paths=_; node_count; edge_count; builder } =
      CICGraph.run ~state updater G.builder_nil Local in
    let context_range = List.map (fun (_, (_, n)) -> n) @@ Id.Map.bindings context_map in
    let find_local_argument = find_local_argument context_map in
    let graph = Request.Predict.graph_init predict in
    CapnpGraphWriter.write_graph graph (function | Local -> 0 | Global -> 1) node_count edge_count builder;
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
          List.map (fun arg ->
              let term = match Argument.get arg with
                | Argument.Undefined _ | Argument.Unresolvable -> raise IllegalArgument
                | Argument.Term t -> t in
              let dep_index = Argument.Term.dep_index_get term in
              let node_index = Stdint.Uint32.to_int @@ Argument.Term.node_index_get term in
              match dep_index with
              | 0 -> find_local_argument node_index
              | 1 -> find_global_argument node_index
              | _ -> raise IllegalArgument
            ) args in
        let preds = Capnp.Array.to_list preds in
        let preds = List.filter_map (fun p ->
            let tac = Prediction.tactic_get p in
            let tid = Tactic.ident_get_int_exn tac in
            let args = convert_args @@ Prediction.arguments_get_list p in
            let tac, params = find_tactic tacs tid in
            if params <> List.length args then raise MismatchedArguments;
            let conf = Prediction.confidence_get p in
            Option.map (fun tac -> tac, conf) @@ Tactic_one_variable.tactic_substitute args tac
          ) preds in
        preds
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 4")

  let drain =
    let drainid = ref 0 in
    fun rc wc ->
      let module Request = Api.Builder.PredictionProtocol.Request in
      let module Response = Api.Reader.PredictionProtocol.Response in
      let request = Request.init_root () in
      let hash = Hashtbl.hash_param 255 255 (!drainid, Unix.gettimeofday (), Unix.getpid ()) in
      Request.synchronize_set_int_exn request hash;
      drainid := !drainid + 1;
      Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message request;
      let rec loop () =
        match Capnp_unix.IO.ReadContext.read_message rc with
        | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3")
        | Some response ->
          let response = Response.of_message response in
          match Response.get response with
          | Response.Synchronized id when Stdint.Uint64.to_int id = hash -> ()
          | _ -> loop () in
      loop ()

  type model =
    { tactics : glob_tactic_expr list
    ; write_context : Unix.file_descr Capnp_unix.IO.WriteContext.t
    ; read_context : Unix.file_descr Capnp_unix.IO.ReadContext.t }

  let connect_socket my_socket =
    { tactics = []
    ; write_context = Capnp_unix.IO.create_write_context_for_fd ~compression:`Packing my_socket
    ; read_context = Capnp_unix.IO.create_read_context_for_fd ~compression:`Packing my_socket }

  let connect_stdin () =
    ThisLogs.info (fun m -> m "%s" "starting proving server on stdin");
    let my_socket, other_socket = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let pid = Unix.create_process
        "pytact-server" [| "pytact-server" |] other_socket Unix.stdout Unix.stderr in
    Declaremods.append_end_library_hook (fun () ->
        Unix.kill pid Sys.sigkill;
        ignore (Unix.waitpid [] pid));
    Unix.close other_socket;
    connect_socket my_socket

  let connect_tcpip ip_addr port =
    ThisLogs.info (fun m -> m "%s %s %d" "connecting to proving server on" ip_addr port);
    let my_socket =  Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let server_addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port) in
    (try
       Unix.connect my_socket server_addr;
       ThisLogs.info (fun m -> m "%s" "connected to python server");
    with
     | Unix.Unix_error (Unix.ECONNREFUSED,s1,s2) -> ThisLogs.err (fun m -> m "%s" "connection to proving server refused")
     | ex ->
         (ThisLogs.err (fun m -> m "%s" "exception caught, closing connection to prover");
          Unix.close my_socket;
          raise ex));

    Declaremods.append_end_library_hook (fun () ->
        Unix.close my_socket;
      );
    connect_socket my_socket



  let empty () = connect_stdin ()
  (* let empty () = connect_tcpip "127.0.0.1" 33333*)

  let learn ({ tactics; _ } as db) _origin _outcomes tac =
    match tac with
    | None -> db
    | Some tac ->
      let tac = tactic_repr tac in
      let tactics = tac::tactics in
      last_model := tactics;
      { db with tactics }
  let predict { tactics; write_context; read_context } =
    drain read_context write_context;
    if not @@ textmode_option () then
      let env = Global.env () in
      let tacs, state = init_predict read_context write_context tactics env in
      let find_global_argument = find_global_argument state in
      fun f ->
        if f = [] then IStream.empty else
          let preds = predict read_context write_context find_global_argument state tacs env
              (List.hd f).state in
          let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
          IStream.of_list preds
    else
      let env = Global.env () in
      init_predict_text read_context write_context;
      fun f ->
        if f = [] then IStream.empty else
          let preds = predict_text read_context write_context env
              (List.hd f).state in
          let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
          IStream.of_list preds
  let evaluate db _ _ = 0., db

end

let () = register_online_learner "Neural Learner" (module NeuralLearner)
