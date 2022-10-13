open Tactician_ltac1_record_plugin
open Graph_def
open Names
open Declarations
open Context
open Ltac_plugin
open Tacexpr

module ProjMap = CMap.Make(Projection.Repr)

let map_named f = function
  | Named.Declaration.LocalAssum (id, ty) ->
    let ty' = f ty in Named.Declaration.LocalAssum (id, ty')
  | Named.Declaration.LocalDef (id, v, ty) ->
    let v' = f v in
    let ty' = f ty in Named.Declaration.LocalDef (id, v', ty')

let with_depth f =
  let old = Topfmt.get_depth_boxes () in
  Fun.protect ~finally:(fun () -> Topfmt.set_depth_boxes old)
    (fun () -> Topfmt.set_depth_boxes (Some 32); f ())

(* Safe version of Learner_helper.proof_state_to_string *)
let proof_state_to_string_safe (hyps, concl) env evar_map =
  let open Context in
  let constr_str t = Pp.string_of_ppcmds (Sexpr.format_oneline (
      Printer.safe_pr_constr_env env evar_map t)) in
  let goal = constr_str concl in
  let id_str id = Names.Id.to_string id.binder_name in
  let hyps = List.rev hyps in
  let hyps = List.map (function
      | Named.Declaration.LocalAssum (id, typ) ->
        id_str id ^ " : " ^ constr_str typ
      | Named.Declaration.LocalDef (id, term, typ) ->
        id_str id ^ " := " ^ constr_str term ^ " : " ^ constr_str typ
    ) hyps in
  String.concat ", " hyps ^ " |- " ^ goal

type single_proof_state = (Constr.t, Constr.t) Named.Declaration.pt list * Constr.t * Evar.t
type proof_state = Evd.evar_map * unit (* single_proof_state Evar.Map.t *) * single_proof_state
type outcome = proof_state * Constr.t * proof_state list
type tactical_proof = (outcome list * glob_tactic_expr option) list
type env_extra = tactical_proof Id.Map.t * tactical_proof Cmap.t

module type CICGraphMonadType = sig

  include Monad.Def
  type node
  type node_label
  type edge_label
  type children = (edge_label * node) list
  type 'a repr_t

  (** Graph drawing primitives *)
  val mk_node : node_label -> children -> node t
  val with_delayed_node : ?definition:bool -> (node -> ('a * node_label * children) t) -> 'a t
  val with_delayed_nodes : ?definition:bool ->int -> (node list -> ('a * (node_label * children) list) t) -> 'a t

  (** Registering and lookup of definitions *)
  type status =
    | NActual
    | NDischarged
    | NSubstituted
  type node_status = status * node

  val register_constant : Constant.t -> node -> unit t
  val find_constant : Constant.t -> node_status option t

  val register_inductive : inductive -> node -> unit t
  val find_inductive : inductive -> node_status option t

  val register_constructor : constructor -> node -> unit t
  val find_constructor : constructor -> node_status option t

  val register_projection : Projection.Repr.t -> node -> unit t
  val find_projection : Projection.Repr.t -> node_status option t

  val register_section_variable : Id.t -> node -> unit t
  val find_section_variable : Id.t -> node option t

  val get_previous_definition : node option t
  val drain_external_previous_definitions : node list t

  (** Managing the local context *)
  val lookup_relative     : int -> node t
  val lookup_named        : Id.t -> node t
      (* The array is the local context associated with the section, in the same ordering as would be given
        by an Evar node. If an element in the node is None, it is a section variable. *)
  val lookup_evar         : Evar.t -> (node * node option array) option t
  val lookup_named_map    : node Id.Map.t t
  val lookup_def_depth    : int option t
  val lookup_def_truncate : bool t
  val lookup_env          : Environ.env t
  val lookup_env_extra    : env_extra t
  val lookup_include_metadata : bool t
  val lookup_include_opaque : bool t

  val with_relative       : node -> 'a t -> 'a t
  val with_relatives      : node list -> 'a t -> 'a t
  val with_named          : Id.t -> node -> 'a t -> 'a t
  val with_evar           : Evar.t -> node -> node option array -> 'a t -> 'a t
  val with_empty_contexts : 'a t -> 'a t
  val with_decremented_def_depth : 'a t -> 'a t
  val with_env            : Environ.env -> 'a t -> 'a t
  val with_env_extra      : env_extra -> 'a t -> 'a t

  type definitions =
    { constants            : node_status Cmap.t
    ; inductives           : node_status Indmap.t
    ; constructors         : node_status Constrmap.t
    ; projections          : node_status ProjMap.t }
  type state =
    { previous : node option
    ; external_previous : node list
    ; section_nodes : node Id.Map.t
    ; definition_nodes : definitions}

  val run :
    ?include_metadata:bool -> ?include_opaque:bool -> ?def_truncate:bool -> ?def_depth:int -> state:state ->
    'a t -> (state * 'a) repr_t
  val run_empty :
    ?include_metadata:bool -> ?include_opaque:bool -> ?def_truncate:bool -> ?def_depth:int ->
    'a t -> (state * 'a) repr_t
end
module CICGraphMonad (G : GraphMonadType) : CICGraphMonadType
  with type node = G.node
   and type node_label = G.node_label
   and type edge_label = G.edge_label
   and type 'a repr_t = 'a G.repr_t = struct

  include G

  type status =
    | NActual
    | NDischarged
    | NSubstituted
  type node_status = status * node
  type definitions =
    { constants            : node_status Cmap.t
    ; inductives           : node_status Indmap.t
    ; constructors         : node_status Constrmap.t
    ; projections          : node_status ProjMap.t }
  type context =
    { relative     : node list
    ; named        : node Id.Map.t
    ; sigma        : (node * node option array) Evar.Map.t
    ; def_depth    : int option
    ; def_truncate : bool
    ; env          : Environ.env option
    ; env_extra    : env_extra option
    ; metadata     : bool
    ; opaque       : bool }
  type state =
    { previous : node option
    ; external_previous : node list
    ; section_nodes : node Id.Map.t
    ; definition_nodes : definitions}

  module M = Monad_util.ReaderStateMonadT
      (G)
      (struct type r = context end)
      (struct type s = state end)
  module OList = List
  open M
  include Monad_util.WithMonadNotations(M)

  (** Lifting of basic drawing primitives *)
  let mk_node nl ch = M.lift @@ mk_node nl ch
  let with_delayed_node ?definition f = unrun @@ fun c s ->
    with_delayed_node ?definition (fun n ->
        G.map (fun (s, (a, nl, ch)) -> (s, a), nl, ch) (run (f n) c s))

  (* This is a typical function that would benefit greatly from having sized vectors! *)
  let with_delayed_nodes ?definition i f =
    let rec aux i nodes =
      if i = 0 then
        f nodes
      else
        with_delayed_node ?definition @@ fun n ->
          let+ a, chs = aux (i - 1) (n::nodes) in
          match chs with
          | [] -> CErrors.anomaly Pp.(str "with_delayed_nodes received to few children nodes")
          | (nl, ch)::chs -> (a, chs), nl, ch
    in
    let+ a, chs = aux i [] in
    match chs with
    | [] -> a
    | _ -> CErrors.anomaly Pp.(str "with_delayed_nodes received to many children nodes")

  (** Registering and lookup of definitions *)

  let update_error x = function
    | None -> Some x
    | Some _ -> CErrors.anomaly (Pp.str "Map update attempt while key already existed")
  let update_error2 p x = function
    | None -> Some x
    | Some (NActual, _) -> CErrors.anomaly Pp.(str "Map update attempt while key already existed: " ++ p)
    | Some ((NDischarged | NSubstituted), _) -> Some x

  let register_constant c n =
    let* ({ definition_nodes = ({ constants; _ } as dn); _ } as s) = get in
    put { s with
          previous = Some n
        ; definition_nodes =
            { dn with constants = Cmap.update c (update_error2 (Constant.print c) (NActual, n)) constants }}
  let find_constant c =
    let+ { definition_nodes = { constants; _ }; _ } = get in
    Cmap.find_opt c constants

  let register_inductive i n =
    let* ({ definition_nodes = ({ inductives; _ } as dn); _ } as s) = get in
    put { s with
          previous = Some n
        ; definition_nodes =
            { dn with inductives = Indmap.update i (update_error2 (Pp.str "ind") (NActual, n)) inductives }}
  let find_inductive i =
    let+ { definition_nodes = { inductives; _ }; _ } = get in
    Indmap.find_opt i inductives

  let register_constructor c n =
    let* ({ definition_nodes = ({ constructors; _ } as dn); _ } as s) = get in
    put { s with
          previous = Some n
        ; definition_nodes =
            { dn with constructors = Constrmap.update c (update_error2 (Pp.str "constr") (NActual, n)) constructors }}
  let find_constructor c =
    let+ { definition_nodes = { constructors; _ }; _ } = get in
    Constrmap.find_opt c constructors

  let register_projection p n =
    let* ({ definition_nodes = ({ projections; _ } as dn); _ } as s) = get in
    put { s with
          previous = Some n
        ; definition_nodes =
            { dn with projections = ProjMap.update p (update_error2 (Pp.str "proj") (NActual, n)) projections  }}
  let find_projection p =
    let+ { definition_nodes = { projections; _ }; _ } = get in
    ProjMap.find_opt p projections

  let register_section_variable id n =
    let* ({ section_nodes; _ } as s) = get in
    put { s with
          previous = Some n
        ; section_nodes = Id.Map.update id (update_error n) section_nodes }
  let find_section_variable id =
    let+ { section_nodes; _ } = get in
    Id.Map.find_opt id section_nodes

  let get_previous_definition =
    let+ { previous; _ } = get in
    previous
  let drain_external_previous_definitions =
    let open Monad.Make(M) in
    let* ({ external_previous; _ } as s) = get in
    let+ () = put { s with external_previous = [] } in
    external_previous

  (** Managing the local context *)
  let lookup_relative i =
    let+ { relative; _ } = ask in
    let rec find ctx i = match ctx, i with
      | node::_, 1 -> node
      | _::ctx, n -> find ctx (n - 1)
      | [], _ -> CErrors.anomaly (Pp.str "Invalid relative context") in
    find relative i
  let lookup_named_map =
    let+ { named; _ } = ask in
    named
  let lookup_named id =
    let+ named = lookup_named_map in
    Id.Map.find id named
  let lookup_evar e =
    let+ { sigma; _ } = ask in
    Evar.Map.find_opt e sigma
  let lookup_def_depth =
    let+ { def_depth; _ } = ask in
    def_depth
  let lookup_def_truncate =
    let+ { def_truncate; _ } = ask in
    def_truncate
  let lookup_env =
    let+ { env; _ } = ask in
    match env with
    | None -> CErrors.anomaly Pp.(str "No env was present in the graph extractor")
    | Some env -> env
  let lookup_env_extra =
    let+ { env_extra; _ } = ask in
    match env_extra with
    | None -> CErrors.anomaly Pp.(str "No extra env was present in the graph extractor")
    | Some env_extra -> env_extra
  let lookup_include_metadata =
    let+ { metadata; _ } = ask in
    metadata
  let lookup_include_opaque =
    let+ { opaque; _ } = ask in
    opaque

  let with_relative n =
    local (fun ({ relative; _ } as c) -> { c with relative = n::relative })
  let with_relatives ns =
    local (fun ({ relative; _ } as c) -> { c with relative = ns@relative })
  let with_named id n =
    local (fun ({ named; _ } as c) -> { c with named = Id.Map.update id (update_error n) named })
  let with_evar e n ns =
    local (fun ({ sigma; _ } as c) -> { c with sigma = Evar.Map.update e (update_error (n, ns)) sigma })
  let with_empty_contexts m =
    local (fun c -> { c with named = Id.Map.empty; sigma = Evar.Map.empty; relative = [] }) m
  let with_decremented_def_depth m =
    local (fun ({ def_depth; _ } as c) ->
        { c with def_depth = Option.map (fun def_depth -> assert (def_depth > 0); def_depth - 1) def_depth }) m
  let with_env env =
    local (fun c -> { c with env = Some env })
  let with_env_extra env_extra =
    local (fun c -> { c with env_extra = Some env_extra })

  let run ?(include_metadata=false) ?(include_opaque=true) ?(def_truncate=false) ?def_depth ~state m =
    G.run @@
    let context =
      { relative = []; named = Id.Map.empty; sigma = Evar.Map.empty
      ; def_depth; def_truncate; env = None; env_extra = None; metadata = include_metadata
      ; opaque = include_opaque } in
    run m context state
  let run_empty ?(include_metadata=false) ?(include_opaque=true) ?(def_truncate=false) ?def_depth m =
    let definition_nodes =
      { constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = ProjMap.empty} in
    let state =
      { previous = None
      ; external_previous = []
      ; section_nodes = Id.Map.empty
      ; definition_nodes } in
    run ~include_metadata ~include_opaque ~def_truncate ?def_depth ~state m
end

module GraphBuilder
    (G : sig
       type node'
       include CICGraphMonadType
         with type node = node'
          and type edge_label = edge_type
          and type node_label = node' node_type
     end) : sig
  open G
  type 'a with_envs = Environ.env -> env_extra -> 'a
  val gen_const               : (constant -> node t) with_envs
  val gen_mutinductive_helper : (mutind -> unit t) with_envs
  val gen_inductive           : (inductive -> node t) with_envs
  val gen_constructor         : (constructor -> node t) with_envs
  val gen_projection          : (Projection.t -> node t) with_envs
  val gen_constr              : (Constr.t -> node t) with_envs
  val gen_section_var         : (Id.t -> node t) with_envs
  val with_named_context      : ((Constr.t, Constr.t) Named.pt -> 'a t -> ((edge_type * node) list * 'a) t) with_envs
  val gen_proof_state         : env_extra -> node t Proofview.tactic
  val gen_globref             : (GlobRef.t -> node t) with_envs
end = struct

  type 'a with_envs = Environ.env -> env_extra -> 'a
  module OList = List
  open G
  open Monad.Make(G)
  open Monad_util.WithMonadNotations(G)

  let cached_gen_const c gen =
    let* n = find_constant c in
    match n with
    | Some (NActual, c) -> return c
    | _ -> with_empty_contexts gen

  let with_named_context gen_constr c (m : 'a t) =
    let* env = lookup_env in
    snd @@ Named.fold_inside (fun (index, m) d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          (try
             ignore (Environ.lookup_named id.binder_name env); (index, m)
           with Not_found ->
             index + 1,
             let* typ = gen_constr typ in
             let* var = mk_node (ContextAssum (index, id.binder_name)) [ContextDefType, typ] in
             let+ (ctx, v) = with_named id.binder_name var m in
             ((ContextElem, var)::ctx), v)
        | Named.Declaration.LocalDef (id, term, typ) ->
          (try
             ignore (Environ.lookup_named id.binder_name env); (index, m)
           with Not_found ->
             index + 1,
             let* typ = gen_constr typ in
             let* term = gen_constr term in
             let* var = mk_node (ContextDef (index, id.binder_name)) [ContextDefType, typ; ContextDefTerm, term] in
             let+ (ctx, v) = with_named id.binder_name var m in
             ((ContextElem, var)::ctx), v))
      c ~init:(0, (let+ m = m in [], m))

  (* This is used for definition leafs when we don't want to 'follow' definitions *)
  let mk_fake_definition def_type gref =
    (* TODO: Using the nametab here is very dangerous because it is mutable *)
    let path = Nametab.path_of_global gref in
    with_delayed_node @@ fun node ->
    return (node,
            Definition { previous = None; external_previous = []; def_type = def_type node;
                         path; status = DOriginal; term_text = None; type_text = "" },
            [])

  let mk_definition (def_type : node definition_type) gref mk_node =
    let* def =
      (* TODO: Using the nametab here is very dangerous because it is mutable *)
      let path = Nametab.path_of_global gref in
      let* previous = get_previous_definition
      and+ status = match def_type with
        | Ind (_, i) -> find_inductive i
        | Construct (_, c) -> find_constructor c
        | Proj (_, p) -> find_projection p
        | ManualConst c | TacticalConstant (c, _) -> find_constant c
        | ManualSectionConst c | TacticalSectionConstant (c, _) -> (* Always actual *) return None
      and+ external_previous = drain_external_previous_definitions
      and+ env = lookup_env in
      let status = match status with
        | None -> DOriginal
        | Some (NActual, _) -> CErrors.anomaly Pp.(str "Definition was already known as an actual while creating it")
        | Some (NDischarged, n) -> DDischarged n
        | Some (NSubstituted, n) -> DSubstituted n in
      let+ type_text, term_text =
        let+ b = lookup_include_metadata in
        if not b then "", None else
          let print c =
            (* This try is specifically aimed at catching errors coming from Coqlib.lib_ref, which is at least
               Known to happen in theories/Float/PrimFloat.v on line 92 *)
            try
              with_depth @@ fun () -> Pp.string_of_ppcmds @@ Sexpr.format_oneline @@
              Printer.pr_constr_env env Evd.empty c
            with CErrors.UserError _ -> ""
          in
          match def_type with
          | Ind (_, (m, i)) ->
            let ({ mind_packets; _ } as mb) =
              Environ.lookup_mind m env in
            let univs = Declareops.inductive_polymorphic_context mb in
            let inst = Univ.make_abstract_instance univs in
            let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) env in
            let typ = Inductive.type_of_inductive env ((mb, Array.get mind_packets i), inst) in
            print typ, None
          | Construct (_, ((m, i), c)) ->
            let ({ mind_packets; _ } as mb) =
              Environ.lookup_mind m env in
            let univs = Declareops.inductive_polymorphic_context mb in
            let inst = Univ.make_abstract_instance univs in
            let typ = Inductive.type_of_constructor (((m, i), c), inst) (mb, Array.get mind_packets i) in
            print typ, None
          (* TODO: Unclear what to do here, but this situation has already changed in newer Coq's *)
          | Proj _ -> "", None
          | ManualConst c | TacticalConstant (c, _) ->
            let { const_body; const_type; _ } = Environ.lookup_constant c env in
            let term_text = match const_body with
              | Undef _ -> None
              | Def c -> Some (print @@ Mod_subst.force_constr c)
              | OpaqueDef c ->
                let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Environ.opaque_tables env) c in
                Some (print c)
              | Primitive p -> None in
            print const_type, term_text
          | ManualSectionConst id | TacticalSectionConstant (id, _) ->
            let def = Environ.lookup_named id env in
            match def with
            | Named.Declaration.LocalAssum (_, typ) ->
              print typ, None
            | Named.Declaration.LocalDef (_, term, typ) ->
              print typ, Some (print term)
      in
      Definition { previous; external_previous; def_type; path; status; type_text; term_text } in
    let* n = mk_node def in
    let+ () = match def_type with
      | Ind (_, i) -> register_inductive i n
      | Construct (_, c) -> register_constructor c n
      | Proj (_, p) -> register_projection p n
      | ManualConst c | TacticalConstant (c, _) -> register_constant c n
      | ManualSectionConst id | TacticalSectionConstant (id, _) -> register_section_variable id n in
    n, def

  let follow_def alt m =
    let* follow_defs = lookup_def_depth in
    match follow_defs with
    | None -> m
    | Some follow_defs ->
      if follow_defs > 0 then with_decremented_def_depth m else alt

  let if_not_truncate alt m =
    let* truncate = lookup_def_truncate in
    if truncate then alt else m

  let rec gen_proof_step (outcomes, tactic) =
    let* env = lookup_env
    and+ metadata = lookup_include_metadata in
    let tactic, args = match tactic with
      | None -> None, []
      | Some tac ->
        let pr_tac t = Pp.string_of_ppcmds @@ Sexpr.format_oneline (Pptactic.pr_glob_tactic env t) in
        let tac_orig = Tactic_name_remove.tactic_name_remove tac in
        let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac_orig in
        let (args, tactic_exact), interm_tactic = Tactic_one_variable.tactic_one_variable tac in
        let base_tactic = Tactic_one_variable.tactic_strip tac in
        let tactic_hash = Hashtbl.hash_param 255 255 base_tactic in
        let tactic =
          if metadata then
            Some { tactic = pr_tac tac_orig
                 ; base_tactic = pr_tac base_tactic
                 ; interm_tactic = pr_tac interm_tactic
                 ; tactic_hash
                 ; tactic_exact }
          else
            Some { tactic = ""
                 ; base_tactic = ""
                 ; interm_tactic = ""
                 ; tactic_hash
                 ; tactic_exact } in
        tactic, args in
    let gen_outcome (((before_sigma, _, (before_hyps, before_concl, before_evar)), term, after) : outcome) =
      let term_text =
        (* TODO: Some evil hacks to work around the fact that we don't have a sigma here *)
        let beauti_evar c =
          let rec aux c =
            match Constr.kind c with
            | Constr.Evar (x, _) -> Constr.mkEvar (x, [||])
            | _ -> Constr.map aux c in
          aux c in
        if metadata then
          Pp.string_of_ppcmds @@ Sexpr.format_oneline @@ Constrextern.with_meta_as_hole (fun () ->
              Flags.with_option Flags.in_toplevel (fun () ->
                  with_depth (fun () ->
                      Printer.safe_pr_constr_env (Global.env()) Evd.empty (beauti_evar term))) ()) ()
        else "" in
      let with_proof_state evd (hyps, concl, evar) m =
        let* ctx, (subject, map) = with_named_context gen_constr hyps @@
          let* subject = gen_constr concl in
          let+ map = lookup_named_map in
          subject, map in
        let* root = mk_node ProofState ((ContextSubject, subject)::ctx) in
        let arr = Array.of_list @@ OList.map (fun id -> Id.Map.find_opt id map) @@
          OList.map Named.Declaration.get_id hyps in
        let+ cont = with_evar evar root arr m in
        (* TODO: Look into what we should give as the evd here *)
        let ps_string = proof_state_to_string_safe (hyps, concl) env evd in
        let context = Id.Map.bindings map in
        { ps_string; root; context; evar }, cont in
      let with_after_states after m =
        OList.fold_left (fun m (evd, _, st) ->
            let+ ps, (ls, res) = with_proof_state evd st m in
            ps::ls, res
          ) (let+ m = m in [], m) after in
      let+ (proof_states_after, (proof_state_before, arguments, term)) =
        with_after_states after @@
        let* ctx, (subject, arguments, context, term) = with_named_context gen_constr before_hyps @@
          let* subject = gen_constr before_concl
          and+ term = gen_constr term
          and+ map = lookup_named_map in
          let warn_arg id = () in
            (* let tac = match tactic with *)
            (*   | None -> "Unknown-tac" *)
            (*   | Some tactic -> tactic.tactic in *)
            (* Feedback.msg_warning Pp.(str "Unknown tactical argument: " ++ Id.print id ++ str " in tactic " ++ *)
            (*                          str tac (\* ++ str " in context\n" ++ *\) *)
            (*                          (\* prlist_with_sep (fun () -> str "\n") (fun (id, node) -> Id.print id ++ str " : ") context *\)) in *)
          let check_default id = function
            | None -> warn_arg id; None
            | x -> x in
          let+ arguments =
            let open Tactic_one_variable in
            List.map (function
                | TVar id ->
                  return @@ check_default id @@
                  Id.Map.find_opt id map
                | TRef c ->
                  (match c with
                   | GlobRef.VarRef id ->
                     (try
                        let def = Environ.lookup_named id env in
                        let+ c = gen_section_var id def in
                        Some c
                      with Not_found ->
                        return @@ check_default id @@
                        Id.Map.find_opt id map)
                   | GlobRef.ConstRef c ->
                     let+ c = gen_const c in
                     Some c
                   | GlobRef.IndRef i ->
                     let+ c = gen_inductive i in
                     Some c
                   | GlobRef.ConstructRef c ->
                     let+ c = gen_constructor c in
                     Some c
                  )
                | TOther -> return None
              ) args in
          let context = Id.Map.bindings map in
          subject, arguments, context, term in
        let+ root = mk_node ProofState ((ContextSubject, subject)::ctx) in
        let ps_string = proof_state_to_string_safe (before_hyps, before_concl) env before_sigma in
        { ps_string; root; context; evar = before_evar }, arguments, term in
      { term; term_text; arguments; proof_state_before; proof_states_after } in
    let+ outcomes = List.map gen_outcome outcomes in
    { tactic; outcomes }
  and gen_const c : G.node t =
    (* Only process canonical constants *)
    let c = Constant.make1 (Constant.canonical c) in
    follow_def (mk_fake_definition (fun _self -> ManualConst c) (GlobRef.ConstRef c)) @@
    cached_gen_const c @@
    let gen_const_aux d p =
      let* env = lookup_env in
      let { const_body; const_type; _ } = Environ.lookup_constant c env in
      let* children = if_not_truncate (return []) @@
        let bt, b = match const_body with
          | Undef _ -> ConstUndef, mk_node ConstEmpty []
          | Def c -> ConstDef, gen_constr @@ Mod_subst.force_constr c
          | OpaqueDef c ->
            let c =
              let* include_opaque = lookup_include_opaque in
              if include_opaque then
                let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Environ.opaque_tables env) c in
                gen_constr c
              else
                mk_node ConstEmpty [] in
            ConstOpaqueDef, c
          | Primitive p -> ConstPrimitive, mk_node (Primitive p) [] in
        let+ b = b
        and+ typ = gen_constr const_type in
        [ ConstType, typ; bt, b ] in
      let+ (n, _) = mk_definition d p (fun d -> mk_node d children) in
      n
    in
    let* proofs = lookup_env_extra in
    let proof = Cmap.find_opt c @@ snd proofs in
    match proof with
    | None -> gen_const_aux (ManualConst c) (GlobRef.ConstRef c)
    | Some proof ->
      let* proof = List.map gen_proof_step proof in
      gen_const_aux (TacticalConstant (c, proof)) (GlobRef.ConstRef c)
  and gen_primitive_constructor representative ind proj_npars typ =
    let relctx, sort = Term.decompose_prod_assum typ in
    let real_arity = Rel.nhyps @@ CList.skipn proj_npars @@ OList.rev relctx in
    snd @@ CList.fold_left (fun (reli, m) -> function
        | Rel.Declaration.LocalAssum ({ binder_name = id; _ }, typ) ->
          reli - 1, with_delayed_node @@ fun prod ->
          (match id with
           | Name id when reli >= 0 ->
             let proj = Projection.Repr.make
                 ind ~proj_npars ~proj_arg:reli @@ Label.of_id id in
             (* TODO: This is clearly incorrect; something needs to be done about this.
                Note that newer versions of Coq already thread projections differently *)
             let const = GlobRef.ConstRef (Projection.Repr.constant proj) in
             let+ _ = mk_definition (Proj (representative, proj)) const (fun d -> mk_node d [ProjTerm, prod]) in
             ()
           | _ -> return ()) >>
          let* typ = gen_constr typ in
          let+ cont = with_relative prod m in
          prod, (Prod id), [ProdType, typ; ProdTerm, cont]
        | Rel.Declaration.LocalDef ({ binder_name = id; _ }, def, typ) ->
          reli, with_delayed_node @@ fun letin ->
          let* typ = gen_constr typ in
          let* def = gen_constr def in
          let+ cont = with_relative letin m in
          letin, (LetIn id), [LetInType, typ; LetInDef, def; LetInTerm, cont])
      (real_arity - 1, gen_constr sort) relctx
  and gen_mutinductive_helper m =
    (* Only process canonical inductives *)
    let m = MutInd.make1 (MutInd.canonical m) in
    let* c = find_inductive (m, 0) in
    match c with
    | Some (NActual, _) -> return ()
    | _ ->
      let* env = lookup_env in

      (* We have to ensure that all dependencies of the inductive are written away before
         we generate the inductive itself. This is needed, because we want all elements of
         the mutually recursive definition cluster to be adjacent in the global context *)
      let dependencies = Definition_order.mutinductive_in_order_traverse env GlobRef.Set.empty
        (fun c set -> GlobRef.Set.add (GlobRef.ConstRef c) set)
        (fun m set -> GlobRef.Set.add (GlobRef.IndRef (m, 0)) set)
        (fun id set -> GlobRef.Set.add (GlobRef.VarRef id) set) m in
      List.iter (fun r -> map (fun _ -> ()) @@ gen_globref r) @@
      GlobRef.Set.elements dependencies >>

      with_empty_contexts @@
      let ({ mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
        Environ.lookup_mind m env in
      let inds = OList.mapi (fun i ind -> i, ind) (Array.to_list mind_packets) in
      with_delayed_nodes ~definition:true (OList.length inds) @@ fun indsn ->
      let representative = OList.hd indsn in
      let inds = OList.combine inds indsn in
      let indsn = OList.rev indsn in (* Backwards ordering w.r.t. Fun *)
      let+ inds = List.map (fun ((i, ({ mind_user_lc; mind_consnames; _ } as ib)), n) ->
          let gen_constr_typ typ = match mind_record with
            | NotRecord | FakeRecord -> gen_constr typ
            | PrimRecord _ -> gen_primitive_constructor representative (m, i) (OList.length mind_params_ctxt) typ in
          let constructs = OList.mapi (fun j x -> j, x) @@
            OList.combine (Array.to_list mind_user_lc) (Array.to_list mind_consnames) in
          let* children =
            let* cstrs = List.map (fun (j, (typ, id)) ->
                with_relatives indsn @@
                let* typ = gen_constr_typ typ in
                let+ n, _ = mk_definition
                    (Construct (representative, ((m, i), j + 1))) (GlobRef.ConstructRef ((m, i), j + 1))
                    (fun d -> mk_node d [ConstructTerm, typ]) in
                IndConstruct, n) constructs in
            let univs = Declareops.inductive_polymorphic_context mb in
            let inst = Univ.make_abstract_instance univs in
            let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) env in
            let typ = Inductive.type_of_inductive env ((mb, ib), inst) in
            let+ n = gen_constr typ in
            (IndType, n)::cstrs in
          let+ _, def = mk_definition (Ind (representative, (m, i))) (GlobRef.IndRef (m, i)) (fun d -> return n) in
          def, children
        ) inds in
      (), inds
  and gen_inductive ((m, _) as i) : G.node t =
    follow_def (mk_fake_definition (fun self -> Ind (self, i)) (GlobRef.IndRef i))
      (gen_mutinductive_helper m >>
       let+ n = find_inductive i in
       match n with
       | Some (NActual, inn) -> inn
       | _ -> CErrors.anomaly (Pp.str "Inductive generation problem"))
  and gen_constructor (((m, _), _) as c) : G.node t =
    follow_def (mk_fake_definition (fun self -> Construct (self, c)) (GlobRef.ConstructRef c))
      (gen_mutinductive_helper m >>
       let+ n = find_constructor c in
       match n with
       | Some (NActual, cn) -> cn
       | _ -> CErrors.anomaly (Pp.str "Inductive generation problem"))
  and gen_projection p =
    (* TODO: This is clearly incorrect; something needs to be done about this.
       Note that newer versions of Coq already treat projections differently *)
    let const = GlobRef.ConstRef (Projection.Repr.constant @@ Projection.repr p) in
    follow_def (mk_fake_definition (fun self -> Proj (self, Projection.repr p)) const)
      (gen_mutinductive_helper (Projection.mind p) >>
       let+ n = find_projection (Projection.repr p) in
       match n with
       | Some (NActual, cn) -> cn
       | _ -> CErrors.anomaly (Pp.str "Inductive generation problem"))
  and gen_section_var id def =
    let* sv = find_section_variable id in
    match sv with
    | None ->
      let gen_aux c =
        follow_def (mk_fake_definition (fun _self -> c) (GlobRef.VarRef id)) @@
        let* children = if_not_truncate (return []) @@
          match def with
          | Named.Declaration.LocalAssum (_, typ) ->
            let+ typ = gen_constr typ
            and+ term = mk_node ConstEmpty [] in
            [ ConstType, typ; ConstUndef, term ]
          | Named.Declaration.LocalDef (_, term, typ) ->
            let+ typ = gen_constr typ
            and+ term = gen_constr term in
            [ ConstType, typ; ConstDef, term ]
        in
        let+ n, _ = mk_definition c (GlobRef.VarRef id) (fun d -> mk_node d children) in
        n in
      let* proofs = lookup_env_extra in
      let proof = Id.Map.find_opt id @@ fst proofs in
      (match proof with
       | None -> gen_aux (ManualSectionConst id)
       | Some proof ->
         let* proof = List.map gen_proof_step proof in
         gen_aux (TacticalSectionConstant (id, proof)))
    | Some sv -> return sv
  and gen_constr c = gen_kind_of_term @@ Constr.kind c
  and gen_kind_of_term = function
    | Rel i ->
      let* ino = lookup_relative i in
      mk_node Rel [RelPointer, ino]
    | Var id ->
      let* env = lookup_env in
      (try
         let def = Environ.lookup_named id env in
         gen_section_var id def
       with Not_found ->
         lookup_named id)
    | Meta i ->
      CErrors.anomaly (Pp.str "Unexpected meta")
    | Evar (ev, substs) ->
      let* subject = lookup_evar ev in
      let ev = Evar.repr ev in
      (match subject with
       | None -> (* TODO: This should be properly resolved at some point *)
         let* undef = mk_node UndefProofState [] in
         mk_node (Evar ev) [ EvarSubject, undef ]
       | Some (n, ctx) ->
         if not (Array.length ctx = Array.length substs) then
           CErrors.anomaly Pp.(str "Array length difference " ++
                               int (Array.length ctx) ++ str " " ++ int (Array.length substs));
         let substs = OList.filter_map (fun (x, y) -> Option.map (fun y -> x, y) y) @@
           OList.combine (Array.to_list substs) (Array.to_list ctx) in
         let* substs = List.map (fun (c, t) ->
             let* c = gen_constr c in
             map (fun n -> EvarSubstPointer, n) @@
             mk_node EvarSubst [EvarSubstTerm, c; EvarSubstTarget, t]) substs in
         mk_node (Evar ev) ((EvarSubject, n)::substs))
    | Sort s ->
      mk_node (match s with
          | Sorts.SProp -> SortSProp
          | Sorts.Prop -> SortProp
          | Sorts.Set -> SortSet
          | Sorts.Type _ -> SortType) []
    | Cast (term, kind, typ) ->
      let* term = gen_constr term in
      let* typ = gen_constr typ in
      mk_node Cast [CastTerm, term; CastType, typ]
    | Prod (id, bi, conc) ->
      let* bi = gen_constr bi in
      with_delayed_node @@ fun prod ->
      let+ conc = with_relative prod @@ gen_constr conc in
      prod, (Prod id.binder_name), [ProdType, bi; ProdTerm, conc]
    | Lambda (id, typ, term) ->
      let* typ = gen_constr typ in
      with_delayed_node @@ fun lambda ->
      let+ term = with_relative lambda @@ gen_constr term in
      lambda, (Lambda id.binder_name), [LambdaType, typ; LambdaTerm, term]
    | LetIn (id, def, typ, term) ->
      let* def = gen_constr def in
      let* typ = gen_constr typ in
      with_delayed_node @@ fun letin ->
      let+ term = with_relative letin @@ gen_constr term in
      letin, (LetIn id.binder_name), [LetInType, typ; LetInDef, def; LetInTerm, term]
    | App (f, args) ->
      let* f = gen_constr f in
      List.fold_left (fun f arg ->
          let* arg = gen_constr arg in
          mk_node App [AppFun, f; AppArg, arg])
          f @@ Array.to_list args
    | Const (c, u) -> gen_const c
    | Ind (i, u) -> gen_inductive i
    | Construct (c, u) -> gen_constructor c
    | Case (i, ret, term, branches) ->
      let* ind = gen_inductive i.ci_ind in
      let* ret = gen_constr ret in
      let* term = gen_constr term in
      let* branches = List.map (fun (c, branch) ->
          let* cstr = gen_constructor (i.ci_ind, c + 1) in
          let* term = gen_constr branch in
          let+ branch = mk_node CaseBranch [CBConstruct, cstr; CBTerm, term] in
          CaseBranchPointer, branch)
          (OList.mapi (fun i x -> i, x) @@ Array.to_list branches) in
      mk_node Case ([CaseInd, ind; CaseReturn, ret; CaseTerm, term]@branches)
    | Fix ((offset, ret), (ids, typs, terms)) ->
      let* funs = with_delayed_nodes (Array.length ids) @@ fun funs ->
        let combined = OList.combine funs @@ OList.combine (Array.to_list ids) @@
          OList.combine (Array.to_list typs) (Array.to_list terms) in
        let+ children = List.map (fun (fn, (id, (typ, term))) ->
            let* typ = gen_constr typ in
            let+ term = with_relatives (OList.rev funs) @@ gen_constr term in
            (FixFun id.binder_name), [FixFunType, typ; FixFunTerm, term])
            combined in
        funs, children in
      mk_node Fix @@ (FixReturn, (OList.nth funs ret))::(OList.map (fun f -> FixMutual, f) funs)
    | CoFix (ret, (ids, typs, terms)) ->
      let* funs = with_delayed_nodes (Array.length ids) @@ fun funs ->
        let combined = OList.combine funs @@ OList.combine (Array.to_list ids) @@
          OList.combine (Array.to_list typs) (Array.to_list terms) in
        let+ children = List.map (fun (fn, (id, (typ, term))) ->
            let* typ = gen_constr typ in
            let+ term = with_relatives (OList.rev funs) @@ gen_constr term in
            (CoFixFun id.binder_name), [CoFixFunType, typ; CoFixFunTerm, term])
            combined in
        funs, children in
      mk_node CoFix @@ (CoFixReturn, (OList.nth funs ret))::(OList.map (fun f -> CoFixMutual, f) funs)
    | Proj (p, term) ->
      let* fn = gen_projection p in
      let* term = gen_constr term in
      mk_node App [AppFun, fn; AppArg, term]
    | Int n ->
      mk_node (Int n) []
    | Float f ->
      mk_node (Float f) []
  and gen_globref = function
    | GlobRef.VarRef id ->
      let* env = lookup_env in
      let def = Environ.lookup_named id env in
      gen_section_var id def
    | GlobRef.ConstRef c -> gen_const c
    | GlobRef.IndRef i -> gen_inductive i
    | GlobRef.ConstructRef c -> gen_constructor c


  let with_named_context ctx m = with_named_context gen_constr ctx m

  let with_envs f env env_extra x = with_env env (with_env_extra env_extra (f x))

  let gen_proof_state env_extra =
    (* NOTE: We intentionally get retrieve then environment here, because we don't want any of the
       local context elements of the proof state to be in the environment. If they are, they will be
       extracted as definitions instead of local variables. *)
    Proofview.tclBIND Proofview.tclENV @@ fun env ->
    Proofview.Goal.enter_one (fun g ->
        let concl = Proofview.Goal.concl g in
        let hyps = Proofview.Goal.hyps g in
        let sigma = Proofview.Goal.sigma g in
        let hyps = OList.map (map_named (EConstr.to_constr sigma)) hyps in
        Proofview.tclUNIT @@
        with_env env @@ with_env_extra env_extra @@
        let* hyps, concl = with_named_context hyps @@ gen_constr (EConstr.to_constr sigma concl) in
        mk_node ProofState ((ContextSubject, concl)::hyps))

  let gen_section_var =
    with_envs @@ fun id ->
    let* env = lookup_env in
    let def = Environ.lookup_named id env in
    gen_section_var id def

  let gen_const = with_envs gen_const
  let gen_mutinductive_helper = with_envs gen_mutinductive_helper
  let gen_inductive = with_envs gen_inductive
  let gen_constructor = with_envs gen_constructor
  let gen_projection = with_envs gen_projection
  let gen_constr = with_envs gen_constr
  let with_named_context env env_extra ps = with_envs (with_named_context ps) env env_extra
  let gen_globref = with_envs gen_globref

end
