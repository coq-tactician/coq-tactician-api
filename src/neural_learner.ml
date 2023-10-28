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
let include_metadata_option = declare_bool_option ~name:"Metadata" ~default:false
let debug_option = declare_bool_option ~name:"Debug" ~default:false
let tcp_option = declare_string_option ~name:"Server" ~default:""
let executable_option = declare_string_option ~name:"Executable" ~default:""
let rpc_option = declare_bool_option ~name:"RPC" ~default:false

open Stdint
module TacticMap = Map.Make(struct type t = int64 let compare = Stdint.Int64.compare end)

let last_model = Summary.ref ~name:"neural-learner-lastmodel" TacticMap.empty

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

type capnp_connection =
  { rc : Unix.file_descr Capnp_unix.IO.ReadContext.t
  ; wc : Unix.file_descr Capnp_unix.IO.WriteContext.t
  ; error_status : unit -> Pp.t option }

(* Whenever we write a message to the server, we prevent a timeout from triggering.
   Otherwise, we might be sending corrupted messages, which causes the server to crash. *)
let write_read_capnp_message_uninterrupted { rc; wc; error_status } m =
  let terminate = ref false in
  let prev_sigterm =
    Sys.signal Sys.sigterm @@ Sys.Signal_handle (fun i ->
        terminate := true) in
  let signals = [Sys.sigalrm; Sys.sigint] in
  ignore (Thread.sigmask Unix.SIG_BLOCK signals);
  try
    Fun.protect ~finally:(fun () ->
        Sys.set_signal Sys.sigterm prev_sigterm;
        ignore (Thread.sigmask Unix.SIG_UNBLOCK signals);
        if !terminate then exit 1) @@ fun () ->
    try
      let module Request = Api.Builder.PredictionProtocol.Request in
      let module Response = Api.Reader.PredictionProtocol.Response in
      Capnp_unix.IO.WriteContext.write_message wc @@ Request.to_message m;
      match Capnp_unix.IO.ReadContext.read_message rc with
      | None -> CErrors.user_err Pp.(str "Cap'n Proto protocol error while communicating with proving server. " ++
                                    str "No response was received.")
      | Some response -> Response.get @@ Response.of_message response
    with Unix.Unix_error (e, _, _) ->
      let error_msg = match error_status () with
        | None -> Pp.mt ()
        | Some err -> Pp.(fnl () ++ str "Connection died with message: " ++ fnl () ++ err) in
      CErrors.user_err Pp.(str "Error while communicating with proving server:" ++ fnl () ++
                          str (Unix.error_message e) ++ error_msg);
  with Fun.Finally_raised e ->
    raise e

let connect_socket my_socket =
  Capnp_unix.IO.create_read_context_for_fd ~compression:`None my_socket,
  Capnp_unix.IO.create_write_context_for_fd ~compression:`None my_socket

let connect_stdin () =
  if debug_option () then
    Feedback.msg_debug Pp.(str "starting proving server with connection through their stdin");
  let my_socket, other_socket = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let mode = if textmode_option () then "text" else "graph" in
  if debug_option () then
    Feedback.msg_debug Pp.(str "using textmode option" ++ str mode);
  let invocation =
    if CString.is_empty @@ executable_option () then
      ["pytact-server"; mode] else
      CString.split_on_char ' ' @@ executable_option () in
  let pid =
    try
        Unix.create_process
          (List.hd invocation) (Array.of_list invocation) other_socket Unix.stdout Unix.stderr
    with Unix.Unix_error _ ->
      CErrors.user_err Pp.(str "Failed to connect to server. Invocation: " ++ str (String.concat " " invocation))
  in
  let error_status () =
    Unix.shutdown my_socket Unix.SHUTDOWN_ALL;
    Unix.close my_socket;
    let pid, status = Unix.waitpid [Unix.WNOHANG] pid in
    if pid == 0 then None else
      match status with
      | Unix.WEXITED 0 -> None
      | Unix.WEXITED 127 ->
        Some Pp.(str "Failed to connect to server. Invocation: " ++ qstring (String.concat " " invocation))
      | Unix.WEXITED c ->  Some Pp.(str "Proving server exited with code " ++ int c)
      | Unix.WSIGNALED c -> Some Pp.(str "Proving server signaled with code " ++ int c)
      | Unix.WSTOPPED c -> Some Pp.(str "Proving server stopped with code " ++ int c) in
  Declaremods.append_end_library_hook (fun () ->
      Unix.shutdown my_socket Unix.SHUTDOWN_ALL;
      Unix.close my_socket;
      ignore (Unix.waitpid [] pid));
  Unix.close other_socket;
  my_socket, error_status

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
  let error_status () =
    Unix.close socket; None in
  Unix.setsockopt socket TCP_NODELAY true; (* Nagles algorithm kills performance, disable *)
  Declaremods.append_end_library_hook (fun () -> Unix.close socket);
  socket, error_status

let log_annotation () =
  let doc = Stm.get_doc 0 in
  let unknown = Pp.str "Unknown location" in
  let loc = Option.cata (fun CAst.{ loc; _ } -> Option.cata Topfmt.pr_loc unknown loc) unknown @@
    Stm.(get_ast ~doc (get_current_state ~doc)) in
  Pp.string_of_ppcmds loc

let init_predict_text add_global_context =
  let module GlobalContextAddition = Api.Builder.GlobalContextAddition in
  let builder init =
    GlobalContextAddition.log_annotation_set init @@ log_annotation ();
    ignore(GlobalContextAddition.data_version_set_reader init Api.Reader.current_version) in
  add_global_context builder

module SDCmap = Symmetric_diff.HMapMake(
  struct
    type t = constant
    include Constant.UserOrd
  end)(
  struct
    include Cmap_env
    module Set = Cset_env
  end)
module SDMindmap = Symmetric_diff.HMapMake(
  struct
    type t = MutInd.t
    include MutInd.UserOrd
  end)(Mindmap_env)
module SDIdmap = Symmetric_diff.Make(Id)(
  struct
    include Id.Map
    module Set = Id.Set
  end)
type context_state =
  { request : (Api.Builder.GlobalContextAddition.t -> unit)
  ; state : CICGraphMonad.state
  ; id : int
  ; constants : Environ.constant_key Cmap_env.t
  ; inductives : Environ.mind_key Mindmap_env.t
  ; section : Constr.named_context }
type context_stack =
  { stack : context_state list
  ; stack_size : int }

let update_context_stack id tacs env { stack_size; stack } =
  let state, old_constants, old_inducives, old_section = match stack with
    | [] ->
      let (empty_state, ()), _ = CICGraphMonad.run_empty (CICGraphMonad.return ())
          (G.HashMap.create 0) G.builder_nil 0 in
      empty_state, Cmap_env.empty, Mindmap_env.empty, []
    | { state; constants; inductives; section; _ }::_ -> state, constants, inductives, section in

  let globals = Environ.Globals.view Environ.(env.env_globals) in
  let section = Environ.named_context env in

  let existing_defs = state.definition_nodes in
  let new_constants = SDCmap.symmetric_diff
      ~eq:(fun _ _ -> true)
      (fun c -> function
         | `Left _ -> fun s -> if Cmap.mem c existing_defs.constants then s else Cset.add c s
         | `Right _ | `Unequal _ -> assert false) globals.constants old_constants Cset.empty in
  let new_inductives = SDMindmap.symmetric_diff
      ~eq:(fun _ _ -> true)
      (fun c -> function
         | `Left _ -> fun s -> if Indmap.mem (c, 0) existing_defs.inductives then s else Mindset.add c s
         | `Right _ | `Unequal _ -> assert false) globals.inductives old_inducives Mindset.empty in
  let new_section =
    if section == old_section then Id.Set.empty else
      let old_section = List.fold_left (fun m pt -> Id.Set.add (Context.Named.Declaration.get_id pt) m)
          Id.Set.empty old_section in
      List.fold_left
        (fun s pt -> let id = Context.Named.Declaration.get_id pt in
          if Id.Set.mem id old_section then s else Id.Set.add id s) Id.Set.empty section in

  if debug_option () then
    Feedback.msg_notice Pp.(
        str "New definitions to be transmitted: " ++ fnl () ++
        pr_vertical_list Constant.print (Cset.elements new_constants)
        ++ pr_vertical_list MutInd.print (Mindset.elements new_inductives)
        ++ pr_vertical_list Id.print (Id.Set.elements new_section)
      );

  if Cset.is_empty new_constants && Mindset.is_empty new_inductives && Id.Set.is_empty new_section &&
     TacticMap.is_empty tacs then state, { stack_size; stack } else

    let { def_count; node_count; edge_count; defs; nodes; edges }, state =
      let open Monad_util.WithMonadNotations(CICGraphMonad) in
      let open Monad.Make(CICGraphMonad) in

      let open GB in
      let env_extra = Id.Map.empty, Cmap.empty in
      let updater =
        let* () = Cset.fold (fun c acc ->
            acc >> let+ _ = gen_const env env_extra c in ()) new_constants (return ()) in
        let* () = Mindset.fold (fun m acc ->
            acc >> gen_mutinductive_helper env env_extra m) new_inductives (return ()) in
        Id.Set.fold (fun id acc ->
            acc >> let+ _ = gen_section_var env env_extra id in ()) new_section (return ()) in
      let (state, ()), builder =
        CICGraphMonad.run ~include_metadata:(include_metadata_option ()) ~include_opaque:false ~state updater
          (G.HashMap.create 100) G.builder_nil stack_size in
      builder, state in
    let node_local_index (_, (def, i)) =
      if def then i else def_count + i in
    let node_hash n = snd @@ G.transform_node_type n in
    let node_label n = fst @@ G.transform_node_type n in
    let builder init =
      let module GlobalContextAddition = Api.Builder.GlobalContextAddition in
      GlobalContextAddition.log_annotation_set init @@ log_annotation ();
      ignore(GlobalContextAddition.data_version_set_reader init Api.Reader.current_version);
      GlobalContextAddition.stack_size_set_int_exn init stack_size;
      let tac_arr = GlobalContextAddition.tactics_init init @@ TacticMap.cardinal tacs in
      List.iteri (fun i (hash, (_tac, params)) ->
          let arri = Capnp.Array.get tac_arr i in
          Api.Builder.AbstractTactic.ident_set arri hash;
          Api.Builder.AbstractTactic.parameters_set_exn arri params)
        (TacticMap.bindings tacs);
      W.write_graph
        ~node_hash ~node_label ~node_lower:(fun n -> fst @@ G.lower n)
        ~node_dep_index:(fun (stack_id, _) -> stack_size - stack_id) ~node_local_index
        ~node_count:(def_count + node_count) ~edge_count (AList.append defs nodes) edges
        ~include_metadata:(include_metadata_option ())
        (GlobalContextAddition.graph_init init);

      let representative = match state.previous with
        | None -> def_count + node_count
        | Some i -> node_local_index @@ fst @@ G.transform_node_type @@ G.lower i in
      GlobalContextAddition.representative_set_int_exn init representative in
    let state = { state with
                  previous = None
                ; external_previous = Option.cata (fun p -> [p]) state.external_previous state.previous } in
    state, { stack_size = stack_size + 1
           ; stack = { request = builder
                     ; state; id
                     ; constants = globals.constants
                     ; inductives = globals.inductives
                     ; section }
                     ::stack }

let context_stack = Summary.ref ~name:"neural-learner-graph-cache"
    { stack = []; stack_size = 0 }
let sync_context_stack add_global_context =
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let id = ref 0 in
  let remote_state = ref [] in
  let remote_stack_size = ref 0 in
  fun ?(keep_cache=true) tacs env ->
    if debug_option () then
      Feedback.msg_notice Pp.(
          str "old remote stack : " ++ prlist_with_sep (fun () -> str "-") int !remote_state ++ fnl () ++
          str "old local stack : " ++ prlist_with_sep (fun () -> str "-")
            (fun { id; _ } -> int id) !context_stack.stack);
    let state, ({ stack_size; stack } as cache) = update_context_stack !id tacs env !context_stack in
    if keep_cache then
      context_stack := cache;
    if debug_option () then
      Feedback.msg_notice Pp.(str "new local stack : " ++ prlist_with_sep (fun () -> str "-")
                                (fun { id; _ } -> int id) cache.stack ++
                              if keep_cache then str " (cached)" else str " (not cached)");
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
          add_global_context request;
          id::rrem
      | _, _ -> assert false in
    remote_state := loop (curtailed_remote_state, stack);
    remote_stack_size := stack_size;
    if debug_option () then
      Feedback.msg_notice Pp.(str "new remote stack : " ++ prlist_with_sep (fun () -> str "-") int !remote_state);
    state, stack_size

type connection =
  { capnp_connection : capnp_connection
  ; sync_context_stack : ?keep_cache:bool -> (glob_tactic_expr * location) TacticMap.t -> Environ.env ->
      CICGraphMonad.state * int }

type communicator =
  { add_global_context : (Api.Builder.GlobalContextAddition.t -> unit) -> unit
  ; sync_context_stack : ?keep_cache:bool -> (glob_tactic_expr * location) TacticMap.t -> Environ.env ->
      CICGraphMonad.state * int
  ; request_prediction : (Api.Builder.PredictionRequest.t -> unit) ->
      (Graph_api.ro, Api.Reader.Prediction.t, Api.Reader.array_t) Capnp.Array.t
  ; request_text_prediction : (Api.Builder.PredictionRequest.t -> unit) ->
      (Graph_api.ro, Api.Reader.TextPrediction.t, Api.Reader.array_t) Capnp.Array.t
  ; check_alignment : unit -> (Graph_api.ro, int64, Api.Reader.array_t) Capnp.Array.t *
                              (Graph_api.ro, Api.Reader.Node.t, Api.Reader.array_t) Capnp.Array.t }


let classify_response_message =
  let module Response = Api.Reader.PredictionProtocol.Response in
  function
  | Response.Initialized -> Pp.str "initialized"
  | Response.Prediction _ -> Pp.str "prediction"
  | Response.TextPrediction _ -> Pp.str "textPrediction"
  | Response.Alignment _ -> Pp.str "alignment"
  | Response.Undefined _ -> Pp.str "unknown"

let protocol_error expected response =
  CErrors.user_err Pp.(str "Cap'n Proto protocol error while communicating with proving server. " ++
                       str "Expected message of type " ++ quote (str expected) ++
                       str " but received message of type " ++
                       quote (classify_response_message response))

let get_communicator =
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let communicator = ref None in
  fun () ->
    match !communicator with
    | None ->
      let socket, error_status =
        if CString.is_empty @@ tcp_option () then
          connect_stdin ()
        else
          let addr = Str.split (Str.regexp ":") (tcp_option()) in
          connect_tcpip (List.nth addr 0) (List.nth addr 1) in
      let rc, wc = connect_socket socket in
      let error_status () =
        let res = error_status () in
        communicator := None;
        res in
      let capnp_connection = { rc; wc; error_status } in
      let add_global_context gca =
        let req = Request.init_root () in
        gca (Request.initialize_init req);
        let response = write_read_capnp_message_uninterrupted capnp_connection req in
        match response with
        | Response.Initialized -> ()
        | _ -> protocol_error "initialized" response in
      let request_prediction rp =
        let req = Request.init_root () in
        rp (Request.predict_init req);
        let response = write_read_capnp_message_uninterrupted capnp_connection req in
        match response with
        | Response.Prediction preds -> preds
        | _ -> protocol_error "prediction" response in
      let request_text_prediction rp =
        let req = Request.init_root () in
        rp (Request.predict_init req);
        let response = write_read_capnp_message_uninterrupted capnp_connection req in
        match response with
        | Response.TextPrediction preds -> preds
        | _ -> protocol_error "textPrediction" response in
      let check_alignment () =
        let req = Request.init_root () in
        Request.check_alignment_set req;
        let response = write_read_capnp_message_uninterrupted capnp_connection req in
        match response with
        | Response.Alignment alignment ->
          Response.Alignment.unaligned_tactics_get alignment,
          Response.Alignment.unaligned_definitions_get alignment
        | _ -> protocol_error "alignment" response in
      let sync_context_stack = sync_context_stack add_global_context in
      let comm = { add_global_context; sync_context_stack; request_prediction
                 ; request_text_prediction; check_alignment } in
      communicator := Some comm;
      comm
    | Some comm -> comm

let check_neural_alignment () =
  let { sync_context_stack; check_alignment; _ } = get_communicator () in
  let module Request = Api.Builder.PredictionProtocol.Request in
  let module Response = Api.Reader.PredictionProtocol.Response in
  let env = Global.env () in
  let tacs = !last_model in
  let state, stack_size = sync_context_stack ~keep_cache:false tacs env in
  let request = Request.init_root () in
  Request.check_alignment_set request;
  let unaligned_tacs, unaligned_defs = check_alignment () in
  let find_global_argument = find_global_argument state in
  let unaligned_tacs = Capnp.Array.map_list ~f:(fun t -> fst @@ find_tactic tacs t) unaligned_tacs in
  let unaligned_defs = Capnp.Array.map_list ~f:(fun node ->
      let sid = stack_size - 1 - Api.Reader.Node.dep_index_get node in
      let nid = Api.Reader.Node.node_index_get_int_exn node in
      find_global_argument (sid, nid)) unaligned_defs in
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

let push_cache () =
  if textmode_option () then () (* No caching needed for the text model at the moment *) else
    let { sync_context_stack; _ } = get_communicator () in
    (* We don't send the list of tactics, hence the empty list. Tactics are only sent right before
       prediction requests are made. *)
    let _, stack_size = sync_context_stack TacticMap.empty (Global.env ()) in
    if debug_option () then
      Feedback.msg_notice Pp.(str "Cache stack size: " ++ int stack_size)

(* TODO: Hack: Options have the property that they are being read by Coq's stm (multiple times) on every
   vernac command. Hence, we can use it to execute arbitrary code. We use to automatically cache. *)
let autocache_option =
  let cache = ref false in
  Goptions.{ optdepr = false
           ; optname = "Tactician Neural Autocache"
           ; optkey = ["Tactician"; "Neural"; "Autocache"]
           ; optread = (fun () -> (if !cache then push_cache () else ()); !cache)
           ; optwrite = (fun v -> cache := v) }
let () = Goptions.declare_bool_option autocache_option

module NeuralLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

  let predict_text request_text_prediction env ps =
    let module Tactic = Api.Reader.Tactic in
    let module Argument = Api.Reader.Argument in
    let module ProofState = Api.Builder.ProofState in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.TextPrediction in
    let module PredictionRequest = Api.Builder.PredictionRequest in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let builder predict =
      let state = PredictionRequest.state_init predict in
      let hyps = List.map (map_named term_repr) @@ proof_state_hypotheses ps in
      let concl = term_repr @@ proof_state_goal ps in
      ProofState.text_set state @@ Graph_extractor.proof_state_to_string_safe (hyps, concl) env Evd.empty in
    let preds = request_text_prediction builder in
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

  let predict request_prediction find_global_argument stack_size state tacs env ps =
    let module Tactic = Api.Reader.Tactic in
    let module Argument = Api.Reader.Argument in
    let module ProofState = Api.Builder.ProofState in
    let module Node = Api.Builder.Node in
    let module Request = Api.Builder.PredictionProtocol.Request in
    let module Prediction = Api.Reader.Prediction in
    let module PredictionRequest = Api.Builder.PredictionRequest in
    let module Response = Api.Reader.PredictionProtocol.Response in
    let updater =
      let open Graph_generator_learner.ConvertStructures(TS) in
      let (_, evm, _) as ps = mk_proof_state ps in
      CICGraphMonad.with_evar_map evm @@
      GB.gen_proof_state env (Names.Id.Map.empty, Names.Cmap.empty) ps in
    let (_, ps), { def_count; node_count; edge_count; defs; nodes; edges } =
      CICGraphMonad.run ~include_metadata:(include_metadata_option ()) ~include_opaque:false ~state updater
        (G.HashMap.create 100) G.builder_nil stack_size in
    let node_local_index (_, (def, i)) =
      if def then i else def_count + i in
    let context_map = List.fold_left (fun map { id; node; _ } ->
        let (p, n), _ = G.lower node in
        Id.Map.add id (p, node_local_index (p, n)) map) Id.Map.empty ps.context in
    let find_local_argument = find_local_argument context_map in
    let node_dep_index (stack_id, _) = stack_size - stack_id in
    let node_hash n = snd @@ G.transform_node_type n in
    let node_label n = fst @@ G.transform_node_type n in
    let request_builder predict =
      let graph = PredictionRequest.graph_init predict in
      W.write_graph
        ~node_hash ~node_label ~node_lower:(fun n -> fst @@ G.lower n)
        ~node_dep_index ~node_local_index
        ~node_count:(def_count + node_count) ~edge_count (AList.append defs nodes) edges graph
        ~include_metadata:(include_metadata_option ());
      let state = PredictionRequest.state_init predict in
      W.write_proof_state
        { node_depindex = (fun n -> node_dep_index (fst @@ G.lower n))
        ; node_local_index = (fun n -> node_local_index (fst @@ G.lower n)) } state ps
        ~include_metadata:(include_metadata_option ()) in
    let preds = request_prediction request_builder in
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

  type model =
    { tactics : (glob_tactic_expr * int) TacticMap.t }

  let empty () =
    { tactics = TacticMap.empty }

  let add_tactic_info env map tac =
    let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac in
    let tac = Tactic_name_remove.tactic_name_remove tac in
    let (args, tactic_exact), interm_tactic = Tactic_one_variable.tactic_one_variable tac in
    let base_tactic = Tactic_one_variable.tactic_strip tac in
    let params = List.length args in
    if params >= 256 then map else
      TacticMap.add
        (Tactic_hash.tactic_hash env base_tactic) (base_tactic, params) map

  let learn { tactics } _origin _outcomes tac =
    match tac with
    | None -> { tactics }
    | Some tac ->
      let tac = tactic_repr tac in
      let tactics = add_tactic_info (Global.env ()) tactics tac in
      last_model := tactics;
      {  tactics }

  let predict { tactics } =
    let { add_global_context; sync_context_stack
        ; request_prediction; request_text_prediction; _ } = get_communicator () in
    let env = Global.env () in
    if not @@ textmode_option () then
      let state, stack_size =
        sync_context_stack ~keep_cache:false tactics env in
      let find_global_argument = find_global_argument state in
      fun f ->
        if f = [] then IStream.empty else
          let preds = predict request_prediction find_global_argument stack_size state tactics env
              (List.hd f).state in
          let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
          IStream.of_list preds
    else
      let () = init_predict_text add_global_context in
      fun f ->
        if f = [] then IStream.empty else
          let preds = predict_text request_text_prediction env
              (List.hd f).state in
          let preds = List.map (fun (t, c) -> { confidence = c; focus = 0; tactic = tactic_make t }) preds in
          IStream.of_list preds
  let evaluate db _ _ = 0., db

end

let () = register_online_learner "Neural Learner" (module NeuralLearner)
