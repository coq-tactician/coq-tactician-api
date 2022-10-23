open Ltac_plugin
open Tactician_ltac1_record_plugin
open Tactic_learner
open Names
open Graph_extractor
open Graph_def
open Tacexpr

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
let tcp_option = declare_string_option ~name:"Server" ~default:""
let executable_option = declare_string_option ~name:"Executable" ~default:""

let last_model = Summary.ref ~name:"neural-learner-lastmodel" []

type location = Local | Global
module Hashable = struct
  type t = location
  let hash _ = XXHasher.with_state @@ fun h -> h
end

(* module G = Graph_generator_learner.GlobalCICGraph(Hashable) *)
module G = Graph_generator_learner.GlobalHashedCICGraph(Hashable)
module CICGraphMonad = struct
  include CICGraphMonad(G)
  type node' = node
end
module GB = GraphBuilder(CICGraphMonad)

exception NoSuchTactic
exception UnknownLocalArgument
exception UnknownArgument of int
exception IllegalArgument

module Api = Graph_api.MakeRPC(Capnp_rpc_lwt)

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
    | None -> raise UnknownLocalArgument
    | Some x -> x

let find_global_argument
    CICGraphMonad.{ section_nodes; definition_nodes = { constants; inductives; constructors; projections }; _ } =
  let open Tactic_one_variable in
  let map = Cmap.fold (fun c (_, n) m ->
      let (_, (_, node)), _ = G.lower n in
      Int.Map.add node (TRef (GlobRef.ConstRef c)) m)
      constants Int.Map.empty in
  let map = Indmap.fold (fun c (_, n) m ->
      let (_, (_, node)), _ = G.lower n in
      Int.Map.add node (TRef (GlobRef.IndRef c)) m)
      inductives map in
  let map = Constrmap.fold (fun c (_, n) m ->
      let (_, (_, node)), _ = G.lower n in
      Int.Map.add node (TRef (GlobRef.ConstructRef c)) m)
      constructors map in
  let map = ProjMap.fold (fun c (_, n) m ->
      let (_, (_, node)), _ = G.lower n in
      (* TODO: At some point we have to deal with this. One possibility is using `Projection.Repr.constant` *)
      Int.Map.add node (TRef (GlobRef.ConstRef (Projection.Repr.constant c))) m)
      projections map in
  let map = Id.Map.fold (fun c n m ->
      let (_, (_, node)), _ = G.lower n in
      Int.Map.add node (TRef (GlobRef.VarRef c)) m)
      section_nodes map in
  fun id ->
    match Int.Map.find_opt id map with
    | None ->
      raise (UnknownArgument id)
    | Some x -> x

(* Whenever we write a message to the server, we prevent a timeout from triggering.
   Otherwise, we might be sending corrupted messages, which causes the server to crash. *)
let write_read_capnp_message_uninterrupted rc wc m =
  let terminate = ref false in
  let prev_sigterm =
    Sys.signal Sys.sigterm @@ Sys.Signal_handle (fun i ->
        terminate := true) in
  ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigalrm]);
  try
    Fun.protect ~finally:(fun () ->
        Sys.set_signal Sys.sigterm prev_sigterm;
        ignore (Thread.sigmask Unix.SIG_UNBLOCK [Sys.sigalrm]);
        if !terminate then exit 1) @@ fun () ->
    Capnp_unix.IO.WriteContext.write_message wc m;
    Capnp_unix.IO.ReadContext.read_message rc
  with Fun.Finally_raised e ->
    raise e

let drain =
  let drainid = ref 0 in
  fun rc wc ->
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let hash = Hashtbl.hash_param 255 255 (!drainid, Unix.gettimeofday (), Unix.getpid ()) in
    Request.synchronize_set_int_exn request hash;
    drainid := !drainid + 1;
    let rec loop msg =
      match msg with
      | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3c")
      | Some response ->
        let response = Response.of_message response in
        match Response.get response with
        | Response.Synchronized id when Stdint.Uint64.to_int id = hash -> ()
        | _ -> loop @@ Capnp_unix.IO.ReadContext.read_message rc in
    loop @@ write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request

let connect_socket my_socket =
  Capnp_unix.IO.create_read_context_for_fd ~compression:`Packing my_socket,
  Capnp_unix.IO.create_write_context_for_fd ~compression:`Packing my_socket

let connect_stdin () =
  Feedback.msg_debug Pp.(str "starting proving server with connection through their stdin");
  let my_socket, other_socket = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let mode = if textmode_option () then "text" else "graph" in
  Feedback.msg_debug Pp.(str "using textmode option" ++ str mode);
  let pid =
    if CString.is_empty @@ executable_option () then
      Unix.create_process
        "pytact-server" [| "pytact-server"; mode |] other_socket Unix.stdout Unix.stderr
    else
      let split = CString.split_on_char ' ' @@ executable_option () in
      Unix.create_process
        (List.hd split) (Array.of_list split) other_socket Unix.stdout Unix.stderr
  in
  let (write_context, read_context) as connection = connect_socket my_socket in
  Declaremods.append_end_library_hook (fun () ->
      drain write_context read_context;
      Unix.shutdown my_socket Unix.SHUTDOWN_SEND;
      Unix.shutdown my_socket Unix.SHUTDOWN_RECEIVE;
      Unix.close my_socket;
      ignore (Unix.waitpid [] pid));
  Unix.close other_socket;
  connection

let pp_addr Unix.{ ai_addr; ai_canonname; _ } =
  let open Pp in
  (match ai_addr with
   | Unix.ADDR_INET (addr, port) -> str (Unix.string_of_inet_addr addr) ++ str ":" ++ int port
   | Unix.ADDR_UNIX s -> str "Unix domain " ++ str s) ++ str " " ++
  str "(canonical: " ++ str ai_canonname ++ str ")"

let connect_tcpip host port =
  Feedback.msg_debug Pp.(str "connecting to proving server on" ++ ws 1 ++ str host ++ str ":" ++ str port);
  let addrs = Unix.(getaddrinfo host port [AI_SOCKTYPE SOCK_STREAM; AI_CANONNAME]) in
  if addrs = [] then
    CErrors.user_err Pp.(str "Could not resolve address" ++ ws 1 ++ str host ++ str ":" ++ str port);
  let rec connect = function
    | [] -> CErrors.user_err Pp.(str "Connection to proving server refused at addresses" ++ fnl () ++
                                 prlist_with_sep fnl pp_addr addrs)
    | addr::addrs ->
      let my_socket =  Unix.(socket addr.ai_family addr.ai_socktype addr.ai_protocol) in
      (try
         Feedback.msg_debug Pp.(str "Attempting to connect to" ++ ws 1 ++ pp_addr addr);
         Unix.(connect my_socket addr.ai_addr);
         Feedback.msg_debug Pp.(str "connected to python server");
         my_socket
       with Unix.Unix_error (Unix.ECONNREFUSED,s1,s2) -> connect addrs) in
  let socket = connect addrs in
  let (read_context, write_context) as connection = connect_socket socket in
  Declaremods.append_end_library_hook (fun () ->
      drain read_context write_context;
      Unix.close socket;
    );
  connection

let get_connection =
  let connection = ref None in
  fun () ->
    match !connection with
    | None ->
      let c =
        if CString.is_empty @@ tcp_option () then
          connect_stdin ()
        else
          let addr = Str.split (Str.regexp ":") (tcp_option()) in
          connect_tcpip (List.nth addr 0) (List.nth addr 1) in
      connection := Some c;
      c
    | Some c -> c

let log_annotation () =
  let doc = Stm.get_doc 0 in
  let unknown = Pp.str "Unknown location" in
  let loc = Option.cata (fun CAst.{ loc; _ } -> Option.cata Topfmt.pr_loc unknown loc) unknown @@
    Stm.(get_ast ~doc (get_current_state ~doc)) in
  Pp.string_of_ppcmds loc

let init_predict_text rc wc =
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let request = Request.init_root () in
  let init = Request.initialize_init request in
  Request.Initialize.log_annotation_set init @@ log_annotation ();
  match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
  | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
  | Some response ->
    let response = Response.of_message response in
    match Response.get response with
    | Response.Initialized -> ()
    | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

let populate_global_context_info tacs env ctacs cgraph cdefinitions =
  let { def_count; node_count; edge_count; defs; nodes; edges }, state =
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
    let section_vars = List.map Context.Named.Declaration.get_id @@ Environ.named_context @@ Global.env () in
    let open Monad_util.WithMonadNotations(CICGraphMonad) in
    let open Monad.Make(CICGraphMonad) in

    let open GB in
    let env_extra = Id.Map.empty, Cmap.empty in
    let updater =
      let* () = List.iter (fun c ->
          let+ _ = gen_const env env_extra c in ()) constants in
      let* () = List.iter (gen_mutinductive_helper env env_extra) minductives in
      List.map (gen_section_var env env_extra) section_vars in
    let (state, _), builder =
      CICGraphMonad.run_empty ~include_opaque:false ~def_truncate:(truncate_option ()) updater
        (G.HashMap.create 100) G.builder_nil Global in
    builder, state in

  let tacs = List.fold_left (fun map tac ->
      let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac in
      let tac = Tactic_name_remove.tactic_name_remove tac in
      let (args, tactic_exact), interm_tactic = Tactic_one_variable.tactic_one_variable tac in
      let base_tactic = Tactic_one_variable.tactic_strip tac in
      TacticMap.add
        (Hashtbl.hash_param 255 255 base_tactic) (base_tactic, List.length args) map)
      TacticMap.empty tacs in
  let tac_arr = ctacs @@ TacticMap.cardinal tacs in
  List.iteri (fun i (hash, (_tac, params)) ->
      let arri = Capnp.Array.get tac_arr i in
      Api.Builder.AbstractTactic.ident_set_int_exn arri hash;
      Api.Builder.AbstractTactic.parameters_set_exn arri params)
    (TacticMap.bindings tacs);

  let node_local_index (_, (def, i)) =
    if def then i else def_count + i in
  let node_hash n = snd @@ G.transform_node_type n in
  let node_label n = fst @@ G.transform_node_type n in
  Graph_capnp_generator.write_graph
    ~node_hash ~node_label ~node_lower:(fun n -> fst @@ G.lower n)
    ~node_dep_index:(fun _ -> 0) ~node_local_index
    ~node_count:(def_count + node_count) ~edge_count (AList.append defs nodes) edges cgraph;

  let definitions =
    let f (_, (_, n)) =
      let (_, (def, n)), _ = G.lower n in
      assert def; Stdint.Uint32.of_int n in
    (List.map f @@ Cmap.bindings state.definition_nodes.constants) @
    (List.map f @@ Indmap.bindings state.definition_nodes.inductives) @
    (List.map f @@ Constrmap.bindings state.definition_nodes.constructors) @
    (List.map f @@ ProjMap.bindings state.definition_nodes.projections) @
    (List.map (fun (_, n) ->
         let (_, (def, n)), _ = G.lower n in
         assert def; Stdint.Uint32.of_int n) @@ Id.Map.bindings state.section_nodes)
  in
  ignore(cdefinitions definitions);
  state, tacs

let init_predict rc wc tacs env =
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let request = Request.init_root () in
  let init = Request.initialize_init request in
  Request.Initialize.log_annotation_set init @@ log_annotation ();
  let state, tacs = populate_global_context_info tacs env
    (Request.Initialize.tactics_init init)
    (Request.Initialize.graph_init init)
    (Request.Initialize.definitions_set_list init) in
  match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
  | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
  | Some response ->
    let response = Response.of_message response in
    match Response.get response with
    | Response.Initialized -> tacs, state
    | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

let check_neural_alignment () =
  let rc, wc = get_connection () in
  drain rc wc;
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let env = Global.env () in
  let request = Request.init_root () in
  let init = Request.check_alignment_init request in
  let state, tacs = populate_global_context_info !last_model env
      (Request.CheckAlignment.tactics_init init)
      (Request.CheckAlignment.graph_init init)
      (Request.CheckAlignment.definitions_set_list init) in
  match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
  | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
  | Some response ->
    let response = Response.of_message response in
    match Response.get response with
    | Response.Alignment alignment ->
      let unaligned_tacs = List.map (fun t -> fst @@ find_tactic tacs @@ Stdint.Uint64.to_int t) @@
        Response.Alignment.unaligned_tactics_get_list alignment in
      let unaligned_defs = List.map (fun d -> find_global_argument state @@ Stdint.Uint32.to_int d) @@
        Response.Alignment.unaligned_definitions_get_list alignment in
      let tacs_msg = if CList.is_empty unaligned_tacs then Pp.mt () else
          Pp.(fnl () ++ str "Unaligned tactics: " ++ fnl () ++
             prlist_with_sep fnl (Pptactic.pr_glob_tactic env) unaligned_tacs) in
      let defs_msg =
        let open Tactic_one_variable in
        if CList.is_empty unaligned_defs then Pp.mt () else
          Pp.(fnl () ++ str "Unaligned definitions: " ++ fnl () ++
              prlist_with_sep fnl
                (function
                  | TVar id -> Id.print id
                  | TRef r -> Libnames.pr_path @@ Nametab.path_of_global r
                  | TOther -> Pp.mt ())
                unaligned_defs) in
      Feedback.msg_notice Pp.(
          str "There are " ++ int (List.length unaligned_tacs) ++ str " unaligned tactics and " ++
          int (List.length unaligned_defs) ++ str " unaligned definitions." ++
          tacs_msg ++ defs_msg
        )
    | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

module NeuralLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

  let gen_proof_state env ps =
    let open GB in
    let hyps = proof_state_hypotheses ps in
    let concl = proof_state_goal ps in
    let hyps = List.map (map_named term_repr) hyps in
    let concl = term_repr concl in
    let open CICGraphMonad in
    let open Monad_util.WithMonadNotations(CICGraphMonad) in
    let* hyps, (concl, map) = with_named_context env (Id.Map.empty, Cmap.empty) hyps @@
      let* map = lookup_named_map in
      let+ concl = gen_constr env (Id.Map.empty, Cmap.empty) concl in
      concl, map in
    let+ root = mk_node ProofState ((ContextSubject, concl)::hyps) in
    root, map

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
    match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3a")
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
    let module Node = Api.Builder.Node in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.PredictionProtocol.Prediction in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let predict = Request.predict_init request in
    let updater = gen_proof_state env ps in
    let (_, (root, context_map)), { def_count; node_count; edge_count; defs; nodes; edges } =
      CICGraphMonad.run ~include_opaque:false ~state updater
        (G.HashMap.create 100) G.builder_nil Local in
    let node_local_index (_, (def, i)) =
      if def then i else def_count + i in
    let context_map = Id.Map.map (fun n ->
        let (p, n), _ = G.lower n in
        p, node_local_index (p, n)) context_map in
    let context_range = List.map (fun (_, (_, n)) -> n) @@
      Id.Map.bindings context_map in
    let find_local_argument = find_local_argument context_map in
    let graph = Request.Predict.graph_init predict in

    let node_dep_index (p, _) = match p with
      | Local -> 0
      | Global -> 1 in
    let node_hash n = snd @@ G.transform_node_type n in
    let node_label n = fst @@ G.transform_node_type n in
    Graph_capnp_generator.write_graph
      ~node_hash ~node_label ~node_lower:(fun n -> fst @@ G.lower n)
      ~node_dep_index ~node_local_index
      ~node_count:(def_count + node_count) ~edge_count (AList.append defs nodes) edges graph;
    let state = Request.Predict.state_init predict in
    let capnp_root = ProofState.root_init state in
    ProofState.Root.dep_index_set_exn capnp_root @@ node_dep_index @@ fst @@ G.lower root;
    ProofState.Root.node_index_set_int_exn capnp_root @@ node_local_index @@ fst @@ G.lower root;
    let context_arr = ProofState.context_init state @@ List.length context_range in
    List.iteri (fun i arg ->
        let arri = Capnp.Array.get context_arr i in
        Node.dep_index_set_exn arri @@ node_dep_index (Local, arg);
        Node.node_index_set_int_exn arri arg
      ) context_range;
    match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3b")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.Prediction preds ->
        let preds = Capnp.Array.to_list preds in
        let preds = CList.filter_map (fun (i, p) ->
            let tac = Prediction.tactic_get p in
            let tid = Tactic.ident_get_int_exn tac in
            let tac, params = find_tactic tacs tid in
            let convert_args args =
              List.mapi (fun j arg ->
                  let term = match Argument.get arg with
                    | Argument.Undefined _ | Argument.Unresolvable -> raise IllegalArgument
                    | Argument.Term t -> t in
                  let dep_index = Argument.Term.dep_index_get term in
                  let node_index = Stdint.Uint32.to_int @@ Argument.Term.node_index_get term in
                  match dep_index with
                  | 0 -> find_local_argument node_index
                  | 1 ->
                    (try
                       find_global_argument node_index
                     with UnknownArgument id ->
                       CErrors.anomaly
                         Pp.(str "Unknown global argument " ++ int id ++ str " at index " ++ int j ++
                             str " for prediction " ++ int i ++ str " which is tactic " ++
                             Pptactic.pr_glob_tactic (Global.env ()) tac ++
                             str " with hash " ++ int tid))
                  | _ -> raise IllegalArgument
                ) args in
            let args = convert_args @@ Prediction.arguments_get_list p in
            if params <> List.length args then begin
              CErrors.anomaly
                Pp.(str "Mismatched argument length for prediction " ++ int i ++ str " which is tactic " ++
                    Pptactic.pr_glob_tactic (Global.env ()) tac ++
                    str " with hash " ++ int tid ++
                    str ". Number of arguments expected: " ++ int params ++
                    str ". Number of argument given: " ++ int (List.length args))
            end;
            let conf = Prediction.confidence_get p in
            Option.map (fun tac -> tac, conf) @@ Tactic_one_variable.tactic_substitute args tac
          ) @@ CList.mapi (fun i x -> i, x) preds in
        preds
      | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 4")

  type model =
    { tactics : glob_tactic_expr list
    ; read_context : Unix.file_descr Capnp_unix.IO.ReadContext.t
    ; write_context : Unix.file_descr Capnp_unix.IO.WriteContext.t }

  let empty () =
    let read_context, write_context = get_connection () in
    { tactics = []; read_context; write_context }

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
