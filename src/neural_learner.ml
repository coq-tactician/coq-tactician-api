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

let textmode_option = declare_bool_option ~name:"Textmode" ~default:false
let debug_option = declare_bool_option ~name:"Debug" ~default:false
let tcp_option = declare_string_option ~name:"Server" ~default:""
let executable_option = declare_string_option ~name:"Executable" ~default:""

let last_model = Summary.ref ~name:"neural-learner-lastmodel" []

type location = int
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
exception UnknownArgument of int * int
exception IllegalArgument

module Api = Graph_api.MakeRPC(Capnp_rpc_lwt)
module W = Graph_capnp_generator.Writer(Api)

open Stdint
module TacticMap = Map.Make(struct type t = int64 let compare = Stdint.Int64.compare end)
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

module IntPairMap = Map.Make(struct
  type t = int * int
  let compare (a1, b1) (a2, b2) =
    let c1 = Int.compare a1 a2 in
    if c1 = 0 then Int.compare b1 b2 else c1
  end)

let find_global_argument
    CICGraphMonad.{ section_nodes; definition_nodes = { constants; inductives; constructors; projections }; _ } =
  let open Tactic_one_variable in
  let map = Cmap.fold (fun c (_, n) m ->
      let (sid, (_, node)), _ = G.lower n in
      IntPairMap.add (sid, node) (TRef (GlobRef.ConstRef c)) m)
      constants IntPairMap.empty in
  let map = Indmap.fold (fun c (_, n) m ->
      let (sid, (_, node)), _ = G.lower n in
      IntPairMap.add (sid, node) (TRef (GlobRef.IndRef c)) m)
      inductives map in
  let map = Constrmap.fold (fun c (_, n) m ->
      let (sid, (_, node)), _ = G.lower n in
      IntPairMap.add (sid, node) (TRef (GlobRef.ConstructRef c)) m)
      constructors map in
  let map = ProjMap.fold (fun c (_, n) m ->
      let (sid, (_, node)), _ = G.lower n in
      (* TODO: At some point we have to deal with this. One possibility is using `Projection.Repr.constant` *)
      IntPairMap.add (sid, node) (TRef (GlobRef.ConstRef (Projection.Repr.constant c))) m)
      projections map in
  let map = Id.Map.fold (fun c n m ->
      let (sid, (_, node)), _ = G.lower n in
      IntPairMap.add (sid, node) (TRef (GlobRef.VarRef c)) m)
      section_nodes map in
  fun (sid, nid) ->
    match IntPairMap.find_opt (sid, nid) map with
    | None ->
      raise (UnknownArgument (sid, nid))
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
  if debug_option () then
    Feedback.msg_debug Pp.(str "starting proving server with connection through their stdin");
  let my_socket, other_socket = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let mode = if textmode_option () then "text" else "graph" in
  if debug_option () then
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
  if debug_option () then
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
         if debug_option () then Feedback.msg_debug Pp.(str "Attempting to connect to" ++ ws 1 ++ pp_addr addr);
         Unix.(connect my_socket addr.ai_addr);
         if debug_option () then Feedback.msg_debug Pp.(str "connected to server");
         my_socket
       with Unix.Unix_error (Unix.ECONNREFUSED,s1,s2) -> connect addrs) in
  let socket = connect addrs in
  let (read_context, write_context) as connection = connect_socket socket in
  Declaremods.append_end_library_hook (fun () ->
      drain read_context write_context;
      Unix.close socket;
    );
  connection

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
  ignore(Request.Initialize.data_version_set_reader init Api.Reader.current_version);
  match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
  | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
  | Some response ->
    let response = Response.of_message response in
    match Response.get response with
    | Response.Initialized -> ()
    | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

type context_state =
  { request : Api.Builder.PredictionProtocol.Request.t
  ; state : CICGraphMonad.state
  ; id : int }
type context_stack =
  { stack : context_state list
  ; stack_size : int }

let update_context_stack id tacs env { stack_size; stack } =
  let state = match stack with
    | [] ->
      let (empty_state, ()), _ = CICGraphMonad.run_empty (CICGraphMonad.return ())
          (G.HashMap.create 0) G.builder_nil 0 in
      empty_state
    | { state; _ }::_ -> state in

  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let request = Request.init_root () in
  let init = Request.initialize_init request in
  Request.Initialize.log_annotation_set init @@ log_annotation ();

  ignore(Request.Initialize.data_version_set_reader init Api.Reader.current_version);
  Request.Initialize.stack_size_set_int_exn init stack_size;
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
    let section_vars = List.map Context.Named.Declaration.get_id @@ Environ.named_context env in
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
      CICGraphMonad.run ~include_opaque:false ~state updater
        (G.HashMap.create 100) G.builder_nil stack_size in
    builder, state in

  let tacs = List.fold_left (fun map tac ->
      let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac in
      let tac = Tactic_name_remove.tactic_name_remove tac in
      let (args, tactic_exact), interm_tactic = Tactic_one_variable.tactic_one_variable tac in
      let base_tactic = Tactic_one_variable.tactic_strip tac in
      TacticMap.add
        (Tactic_hash.tactic_hash env base_tactic) (base_tactic, List.length args) map)
      TacticMap.empty tacs in

  let tac_arr = Request.Initialize.tactics_init init @@ TacticMap.cardinal tacs in
  List.iteri (fun i (hash, (_tac, params)) ->
      let arri = Capnp.Array.get tac_arr i in
      Api.Builder.AbstractTactic.ident_set arri hash;
      Api.Builder.AbstractTactic.parameters_set_exn arri params)
    (TacticMap.bindings tacs);

  let node_local_index (_, (def, i)) =
    if def then i else def_count + i in
  let node_hash n = snd @@ G.transform_node_type n in
  let node_label n = fst @@ G.transform_node_type n in
  W.write_graph
    ~node_hash ~node_label ~node_lower:(fun n -> fst @@ G.lower n)
    ~node_dep_index:(fun (stack_id, _) -> stack_size - stack_id) ~node_local_index
    ~node_count:(def_count + node_count) ~edge_count (AList.append defs nodes) edges
    (Request.Initialize.graph_init init);

  let representative = match state.previous with
    | None -> def_count + node_count
    | Some i -> node_local_index @@ fst @@ G.transform_node_type @@ G.lower i in
  Request.Initialize.representative_set_int_exn init representative;
  let state = { state with
                previous = None
              ; external_previous = Option.fold_left (fun ls x -> x::ls) [] state.previous } in
  tacs, state, { stack_size = stack_size + 1; stack = { request; state; id }::stack }

let sync_context_stack rc wc =
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let id = ref 0 in
  let context_stack = Summary.ref ~name:"neural-learner-graph-cache"
      { stack = []; stack_size = 0 } in
  let remote_state = ref [] in
  let remote_stack_size = ref 0 in
  fun tacs env ->
    if debug_option () then
      Feedback.msg_notice Pp.(
          str "old remote stack : " ++ prlist_with_sep (fun () -> str "-") int !remote_state ++ fnl () ++
          str "old local stack : " ++ prlist_with_sep (fun () -> str "-")
            (fun { id; _ } -> int id) !context_stack.stack);
    let tacs, state, ({ stack_size; stack } as cache) = update_context_stack !id tacs env !context_stack in
    context_stack := cache;
    if debug_option () then
      Feedback.msg_notice Pp.(str "new local stack : " ++ prlist_with_sep (fun () -> str "-")
                                (fun { id; _ } -> int id) !context_stack.stack);
    id := !id + 1;
    let stack_diff = !remote_stack_size - stack_size in
    let curtailed_remote_state = if stack_diff > 0 then
        CList.skipn stack_diff !remote_state
      else
        CList.addn (abs stack_diff) (-1) !remote_state in
    let rec loop = function
      | [], [] -> []
      | rid::rrem, { id; request; _ }::lrem ->
        if rid = id then rid::rrem else
          let rrem = loop (rrem, lrem) in
          if debug_option () then
            Feedback.msg_notice Pp.(str "writing message id " ++ int id);
          (match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
          | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
          | Some response ->
            let response = Response.of_message response in
            match Response.get response with
            | Response.Initialized -> ()
            | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2"));
          id::rrem
      | _, _ -> assert false in
    remote_state := loop (curtailed_remote_state, stack);
    remote_stack_size := stack_size;
    if debug_option () then
      Feedback.msg_notice Pp.(str "new remote stack : " ++ prlist_with_sep (fun () -> str "-") int !remote_state);
    tacs, state, stack_size

type connection =
  { rc : Unix.file_descr Capnp_unix.IO.ReadContext.t
  ; wc : Unix.file_descr Capnp_unix.IO.WriteContext.t
  ; sync_context_stack : glob_tactic_expr list -> Environ.env ->
      (glob_tactic_expr * location) TacticMap.t * CICGraphMonad.state * int }

let get_connection =
  let connection = ref None in
  fun () ->
    match !connection with
    | None ->
      let rc, wc =
        if CString.is_empty @@ tcp_option () then
          connect_stdin ()
        else
          let addr = Str.split (Str.regexp ":") (tcp_option()) in
          connect_tcpip (List.nth addr 0) (List.nth addr 1) in
      let conn = { rc; wc; sync_context_stack = sync_context_stack rc wc } in
      connection := Some conn;
      conn
    | Some conn -> conn

let check_neural_alignment () =
  let { rc; wc; sync_context_stack } = get_connection () in
  drain rc wc;
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let env = Global.env () in
  let tacs, state, stack_size = sync_context_stack !last_model env in
  let request = Request.init_root () in
  Request.check_alignment_set request;
  match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
  | None -> CErrors.anomaly Pp.(str "Capnp protocol error 1")
  | Some response ->
    let response = Response.of_message response in
    match Response.get response with
    | Response.Alignment alignment ->
      let find_global_argument = find_global_argument state in
      let unaligned_tacs = List.map (fun t -> fst @@ find_tactic tacs t) @@
        Response.Alignment.unaligned_tactics_get_list alignment in
      let unaligned_defs = List.map (fun node ->
          let sid = stack_size - 1 - Api.Reader.Node.dep_index_get node in
          let nid = Api.Reader.Node.node_index_get_int_exn node in
          find_global_argument (sid, nid)) @@
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
      let def_count = Id.Map.cardinal state.section_nodes +
                      Cmap.cardinal state.definition_nodes.constants +
                      Indmap.cardinal state.definition_nodes.inductives +
                      Constrmap.cardinal state.definition_nodes.constructors +
                      ProjMap.cardinal state.definition_nodes.projections in
      Feedback.msg_notice Pp.(
          str "There are " ++ int (List.length unaligned_tacs) ++ str "/" ++
          int (TacticMap.cardinal tacs) ++ str " unaligned tactics and " ++
          int (List.length unaligned_defs) ++ str "/" ++
          int def_count ++ str " unaligned definitions." ++
          tacs_msg ++ defs_msg
        )
    | _ -> CErrors.anomaly Pp.(str "Capnp protocol error 2")

let push_cache () =
  if textmode_option () then () (* No caching needed for the text model at the moment *) else
    let { rc; wc; sync_context_stack } = get_connection () in
    drain rc wc;
    let _, _, stack_size = sync_context_stack !last_model (Global.env ()) in
    if debug_option () then
      Feedback.msg_notice Pp.(str "Cache stack size: " ++ int stack_size)

module NeuralLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

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

  let predict rc wc find_global_argument stack_size state tacs env ps =
    let module Tactic = Api.Reader.Tactic in
    let module Argument = Api.Reader.Argument in
    let module ProofState = Api.Builder.ProofState in
    let module Node = Api.Builder.Node in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.PredictionProtocol.Prediction in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let request = Request.init_root () in
    let predict = Request.predict_init request in
    let updater =
      let open Graph_generator_learner.ConvertStructures(TS) in
      let (_, evm, _) as ps = mk_proof_state ps in
      CICGraphMonad.with_evar_map evm @@
      GB.gen_proof_state env (Names.Id.Map.empty, Names.Cmap.empty) ps in
    let (_, ps), { def_count; node_count; edge_count; defs; nodes; edges } =
      CICGraphMonad.run ~include_opaque:false ~state updater
        (G.HashMap.create 100) G.builder_nil stack_size in
    let node_local_index (_, (def, i)) =
      if def then i else def_count + i in
    let context_map = List.fold_left (fun map { id; node; _ } ->
        let (p, n), _ = G.lower node in
        Id.Map.add id (p, node_local_index (p, n)) map) Id.Map.empty ps.context in
    let find_local_argument = find_local_argument context_map in
    let graph = Request.Predict.graph_init predict in

    let node_dep_index (stack_id, _) = stack_size - stack_id in
    let node_hash n = snd @@ G.transform_node_type n in
    let node_label n = fst @@ G.transform_node_type n in
    W.write_graph
      ~node_hash ~node_label ~node_lower:(fun n -> fst @@ G.lower n)
      ~node_dep_index ~node_local_index
      ~node_count:(def_count + node_count) ~edge_count (AList.append defs nodes) edges graph;
    let state = Request.Predict.state_init predict in
    W.write_proof_state
      { node_depindex = (fun n -> node_dep_index (fst @@ G.lower n))
      ; node_local_index = (fun n -> node_local_index (fst @@ G.lower n)) } state ps;
    match write_read_capnp_message_uninterrupted rc wc @@ Request.to_message request with
    | None -> CErrors.anomaly Pp.(str "Capnp protocol error 3b")
    | Some response ->
      let response = Response.of_message response in
      match Response.get response with
      | Response.Prediction preds ->
        let preds = Capnp.Array.to_list preds in
        let preds = CList.filter_map (fun (i, p) ->
            let tac = Prediction.tactic_get p in
            let tid = Tactic.ident_get tac in
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
                  | _ ->
                    (try
                       find_global_argument (stack_size - dep_index, node_index)
                     with UnknownArgument (sid, nid) ->
                       CErrors.anomaly
                         Pp.(str "Unknown global argument (" ++ int sid ++ str ", " ++ int nid ++ str ") at index "
                             ++ int j ++ str " for prediction " ++ int i ++ str " which is tactic " ++
                             Pptactic.pr_glob_tactic (Global.env ()) tac ++
                             str " with hash " ++ str (Stdint.Int64.to_string tid)))
                ) args in
            let args = convert_args @@ Prediction.arguments_get_list p in
            if params <> List.length args then begin
              CErrors.anomaly
                Pp.(str "Mismatched argument length for prediction " ++ int i ++ str " which is tactic " ++
                    Pptactic.pr_glob_tactic (Global.env ()) tac ++
                    str " with hash " ++ str (Stdint.Int64.to_string tid) ++
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
    ; connection : connection }

  let empty () =
    let connection = get_connection () in
    { tactics = []; connection }

  let learn ({ tactics; _ } as db) _origin _outcomes tac =
    match tac with
    | None -> db
    | Some tac ->
      let tac = tactic_repr tac in
      let tactics = tac::tactics in
      last_model := tactics;
      { db with tactics }
  let predict { tactics; connection = { rc; wc; sync_context_stack } } =
    drain rc wc;
    let env = Global.env () in
    if not @@ textmode_option () then
      let tacs, state, stack_size = sync_context_stack tactics env in
      let find_global_argument = find_global_argument state in
      fun f ->
        if f = [] then IStream.empty else
          let preds = predict rc wc find_global_argument stack_size state tacs env
              (List.hd f).state in
          let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
          IStream.of_list preds
    else
      let () = init_predict_text rc wc in
      fun f ->
        if f = [] then IStream.empty else
          let preds = predict_text rc wc env
              (List.hd f).state in
          let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
          IStream.of_list preds
  let evaluate db _ _ = 0., db

end

let () = register_online_learner "Neural Learner" (module NeuralLearner)
