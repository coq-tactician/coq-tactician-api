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


type env_extra = (((Constr.t, Constr.t) Named.Declaration.pt list * Constr.t) list * glob_tactic_expr) list Cmap.t

module type CICGraphMonadType = sig

  include Monad.Def
  type node
  type node_label
  type edge_label
  type children = (edge_label * node) list
  type 'a repr_t

  (** Graph drawing primitives *)
  val mk_node : node_label -> children -> node t
  val with_delayed_node : (node -> ('a * node_label * children) t) -> 'a t
  val with_delayed_nodes : int -> (node list -> ('a * (node_label * children) list) t) -> 'a t

  (** Registering and lookup of definitions *)
  val register_constant : Constant.t -> node -> unit t
  val find_constant : Constant.t -> node option t

  val register_inductive : inductive -> node -> unit t
  val find_inductive : inductive -> node option t

  val register_constructor : constructor -> node -> unit t
  val find_constructor : constructor -> node option t

  val register_projection : Projection.Repr.t -> node -> unit t
  val find_projection : Projection.Repr.t -> node option t

  (** Managing the local context *)
  val lookup_relative     : int -> node t
  val lookup_named        : Id.t -> node t
  val lookup_named_map    : node Id.Map.t t
  val lookup_def_depth    : int option t
  val lookup_def_truncate : bool t
  val lookup_env          : Environ.env t
  val lookup_env_extra    : env_extra t

  val get_named_context   : node Id.Map.t t

  val with_relative       : node -> 'a t -> 'a t
  val with_relatives      : node list -> 'a t -> 'a t
  val with_named          : Id.t -> node -> 'a t -> 'a t
  val with_empty_contexts : 'a t -> 'a t
  val with_decremented_def_depth : 'a t -> 'a t
  val with_env            : Environ.env -> 'a t -> 'a t
  val with_env_extra      : env_extra -> 'a t -> 'a t

  type definitions =
    { constants            : node Cmap.t
    ; inductives           : node Indmap.t
    ; constructors         : node Constrmap.t
    ; projections          : node ProjMap.t }

  val run :
    ?def_truncate:bool -> ?def_depth:int ->
    known_definitions:definitions ->
    'a t -> (definitions * 'a) repr_t
  val run_empty :
    ?def_truncate:bool -> ?def_depth:int ->
    'a t -> (definitions * 'a) repr_t
end
module CICGraphMonad (G : GraphMonadType) : CICGraphMonadType
  with type node = G.node
   and type node_label = G.node_label
   and type edge_label = G.edge_label
   and type 'a repr_t = 'a G.repr_t = struct

  include G

  type definitions =
    { constants            : node Cmap.t
    ; inductives           : node Indmap.t
    ; constructors         : node Constrmap.t
    ; projections          : node ProjMap.t }
  type context =
    { relative     : node list
    ; named        : node Id.Map.t
    ; def_depth    : int option
    ; def_truncate : bool
    ; env          : Environ.env option
    ; env_extra    : env_extra option }

  module M = Monad_util.ReaderStateMonadT
      (G)
      (struct type r = context end)
      (struct type s = definitions end)
  module OList = List
  open M
  include Monad_util.WithMonadNotations(M)

  (** Lifting of basic drawing primitives *)
  let mk_node nl ch = M.lift @@ mk_node nl ch
  let with_delayed_node f = unrun @@ fun c s ->
    with_delayed_node (fun n ->
        G.map (fun (s, (a, nl, ch)) -> (s, a), nl, ch) (run (f n) c s))

  (* This is a typical function that would benefit greatly from having sized vectors! *)
  let with_delayed_nodes i f =
    let rec aux i nodes =
      if i = 0 then
        f nodes
      else
        with_delayed_node @@ fun n ->
          let+ a, chs = aux (i - 1) (n::nodes) in
          match chs with
          | [] -> CErrors.anomaly Pp.(str "with_delayed_nodes received to few children nodes")
          | (nl, ch)::chs -> (a, chs), nl, ch
    in
    let+ a, chs = aux i [] in
    match chs with
    | [] -> a
    | _ -> CErrors.anomaly Pp.(str "with_delayed_nodes received to many children nodes")

  let get_named_context =
    let+ context = ask in
    context.named

  (** Registering and lookup of definitions *)

  let update_error x = function
    | None -> Some x
    | Some _ -> CErrors.anomaly (Pp.str "Map update attempt while key already existed")

  let option_map f o =
    match o with
    | None -> return None
    | Some x -> let+ x = f x in Some x
  let with_register_external n =
    let+ () = lift @@ register_external n in
    n

  let register_constant c n =
    let* ({ constants; _ } as s) = get in
    put { s with constants = Cmap.update c (update_error n) constants }
  let find_constant c =
    let* { constants; _ } = get in
    option_map with_register_external @@ Cmap.find_opt c constants

  let register_inductive i n =
    let* ({ inductives; _ } as s) = get in
    put { s with inductives = Indmap.update i (update_error n) inductives }
  let find_inductive i =
    let* { inductives; _ } = get in
    option_map with_register_external @@ Indmap.find_opt i inductives

  let register_constructor c n =
    let* ({ constructors; _ } as s) = get in
    put { s with constructors = Constrmap.update c (update_error n) constructors }
  let find_constructor c =
    let* { constructors; _ } = get in
    option_map with_register_external @@ Constrmap.find_opt c constructors

  let register_projection p n =
    let* ({ projections; _ } as s) = get in
    put { s with projections = ProjMap.update p (update_error n) projections }
  let find_projection p =
    let* { projections; _ } = get in
    option_map with_register_external @@ ProjMap.find_opt p projections

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

  let with_relative n =
    local (fun ({ relative; _ } as c) -> { c with relative = n::relative })
  let with_relatives ns =
    local (fun ({ relative; _ } as c) ->
        let relative = OList.fold_left (fun ctx n -> n::ctx) ns relative in (* Funs are added backwards *)
        { c with relative })
  let with_named id n =
    local (fun ({ named; _ } as c) -> { c with named = Id.Map.update id (update_error n) named })
  let with_empty_contexts m =
    local (fun c -> { c with named = Id.Map.empty; relative = [] }) m
  let with_decremented_def_depth m =
    local (fun ({ def_depth; _ } as c) ->
        { c with def_depth = Option.map (fun def_depth -> assert (def_depth > 0); def_depth - 1) def_depth }) m
  let with_env env =
    local (fun c -> { c with env = Some env })
  let with_env_extra env_extra =
    local (fun c -> { c with env_extra = Some env_extra })

  let run ?(def_truncate=false) ?def_depth ~known_definitions m =
    G.run @@
    let context =
      { relative = []; named = Id.Map.empty; def_depth; def_truncate; env = None; env_extra = None } in
    run m context known_definitions
  let run_empty ?(def_truncate=false) ?def_depth m =
    let known_definitions =
      { constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = ProjMap.empty} in
    run ~def_truncate ?def_depth ~known_definitions m
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
  val gen_inductive           : (inductive -> node' t) with_envs
  val gen_constructor         : (constructor -> node t) with_envs
  val gen_projection          : (Projection.t -> node t) with_envs
  val gen_constr              : (Constr.t -> node t) with_envs
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
    | Some c -> return c
    | None ->
      let* n = with_empty_contexts gen in
      let+ () = register_constant c n in
      n

  let with_named_context gen_constr c (m : 'a t) =
    Named.fold_inside (fun m d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          let* typ = gen_constr typ in
          let* var = mk_node (ContextAssum id.binder_name) [ContextDefType, typ] in
          let+ (ctx, v) = with_named id.binder_name var m in
          ((ContextElem, var)::ctx), v
        | Named.Declaration.LocalDef (id, term, typ) ->
          let* typ = gen_constr typ in
          let* term = gen_constr term in
          let* var = mk_node (ContextDef id.binder_name) [ContextDefType, typ; ContextDefTerm, term] in
          let+ (ctx, v) = with_named id.binder_name var m in
          ((ContextElem, var)::ctx), v)
      c ~init:(let+ m = m in [], m)

  let mk_definition def_type = Definition { previous = []; def_type }

  (* Safe version of Learner_helper.proof_state_to_string *)
  let proof_state_to_string_safe (hyps, concl) env evar_map =
    let open Context in
    let constr_str t = Pp.string_of_ppcmds (Sexpr.format_oneline (
        Printer.safe_pr_constr_env env evar_map t)) in
    let goal = constr_str concl in
    let id_str id = Names.Id.to_string id.binder_name in
    let hyps = OList.rev hyps in
    let hyps = OList.map (function
        | Named.Declaration.LocalAssum (id, typ) ->
          id_str id ^ " : " ^ constr_str typ
        | Named.Declaration.LocalDef (id, term, typ) ->
          id_str id ^ " := " ^ constr_str term ^ " : " ^ constr_str typ
      ) hyps in
    String.concat ", " hyps ^ " |- " ^ goal

  let follow_def alt m =
    let* follow_defs = lookup_def_depth in
    match follow_defs with
    | None -> m
    | Some follow_defs ->
      if follow_defs > 0 then with_decremented_def_depth m else mk_node alt []

  let if_not_truncate alt m =
    let* truncate = lookup_def_truncate in
    if truncate then alt else m

  let rec gen_const c : G.node t =
    (* Only process canonical constants *)
    let c = Constant.make1 (Constant.canonical c) in
    follow_def (mk_definition @@ ManualConst c) @@
    cached_gen_const c @@
    let* proofs = lookup_env_extra in
    let proof = Cmap.find_opt c proofs in
    let gen_const_aux d =
      let* env = lookup_env in
      let { const_hyps; const_body; const_type; _ } = Environ.lookup_constant c env in
      let* children = if_not_truncate (return []) @@
        let+ ctx, b = with_named_context gen_constr const_hyps @@
          let* typ = gen_constr const_type in
          let bt, b = match const_body with
            | Undef _ -> ConstUndef, mk_node ConstEmpty []
            | Def c -> ConstDef, gen_constr @@ Mod_subst.force_constr c
            | OpaqueDef c ->
              let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Environ.opaque_tables env) c in
              ConstOpaqueDef, gen_constr c
            | Primitive p -> ConstPrimitive, mk_node (Primitive p) [] in
          let+ b = b in
          [ ConstType, typ; bt, b ] in
        b@ctx in
      mk_node (mk_definition d) children
    in
    let gen_tactical_proof (ps, tac) =
      let gen_proof_state (hyps, concl) =
        let+ ctx, (subject, map) = with_named_context gen_constr hyps @@
          let* subject = gen_constr concl in
          let+ map = lookup_named_map in
          subject, map in
        ((ContextSubject, subject)::ctx), map in
      let* children, context_map = gen_proof_state ps in
      let* root = mk_node Root children in
      let tac_orig = Tactic_name_remove.tactic_name_remove tac in
      let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac_orig in
      let (args, tactic_exact), interm_tactic = Tactic_one_variable.tactic_one_variable tac in
      let base_tactic = Tactic_one_variable.tactic_strip tac in
      let context_range = OList.map (fun (_, n) -> n) @@ Id.Map.bindings context_map in
      let* env = lookup_env in
      let warn_arg id =
        Feedback.msg_warning Pp.(str "Unknown tactical argument: " ++ Id.print id ++ str " in tactic " ++
                                 Pptactic.pr_glob_tactic env tac_orig (* ++ str " in context\n" ++ *)
                                 (* prlist_with_sep (fun () -> str "\n") (fun (id, node) -> Id.print id ++ str " : ") context *)) in
      let check_default id = function
        | None -> warn_arg id; None
        | x -> x in
      let+ arguments =
        let open Tactic_one_variable in
        List.map (function
            | TVar id ->
              return @@ check_default id @@
              Id.Map.find_opt id context_map
            | TRef c ->
              (match c with
               | GlobRef.VarRef id ->
                 (* TODO: Currently this is not reversible, because we cannot detect the difference between
                    TVar _ and TRef (VarRef _). This should be fixable by making section variables not part
                    of the local context. *)
                 return @@ check_default id @@
                 Id.Map.find_opt id context_map
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
      (* TODO: Look into what we should give as the evd here *)
      let ps_string = proof_state_to_string_safe ps env Evd.empty in
      { tactic = tac_orig; base_tactic; interm_tactic
      ; tactic_hash = Hashtbl.hash_param 255 255 base_tactic
      ; arguments; tactic_exact
      ; root; context = context_range; ps_string } in
    match proof with
    | None -> gen_const_aux (ManualConst c)
    | Some proof ->
      let proof = OList.concat @@ OList.map (fun (pss, tac) -> OList.map (fun ps -> ps, tac) pss) proof in
      let* proof = List.map gen_tactical_proof proof in
      gen_const_aux (TacticalConstant (c, proof))
  and gen_primitive_constructor ind proj_npars typ =
    let relctx, sort = Term.decompose_prod_assum typ in
    let real_arity = Rel.nhyps @@ CList.skipn proj_npars @@ OList.rev relctx in
    snd @@ CList.fold_left (fun (reli, m) -> function
        | Rel.Declaration.LocalAssum ({ binder_name = id; _ }, typ) ->
          reli - 1, with_delayed_node @@ fun prod ->
          (match id with
           | Name id when reli >= 0 ->
             let proj = Projection.Repr.make
                 ind ~proj_npars ~proj_arg:reli @@ Label.of_id id in
             let* nproj = mk_node (mk_definition (Proj proj)) [ProjTerm, prod] in
             register_projection proj nproj
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
      (real_arity - 1, (gen_constr sort)) relctx
  and gen_mutinductive_helper m =
    (* Only process canonical inductives *)
    let m = MutInd.make1 (MutInd.canonical m) in
    let* c = find_inductive (m, 0) in
    match c with
    | Some mn -> return ()
    | None ->
      with_empty_contexts @@
      let* env = lookup_env in
      let ({ mind_hyps; mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
        Environ.lookup_mind m env in
      (* TODO: The elements of this named context were connected to the global root node before. This is not correct.
         Now it is not connected to anything (children are ignored), which is probably also suboptimal. This will hopefully be solved by
         converting this named context to be part of the global context. *)
      map snd @@ with_named_context gen_constr mind_hyps @@
      let inds = OList.mapi (fun i ind -> i, ind) (Array.to_list mind_packets) in
      with_delayed_nodes (OList.length inds) @@ fun indsn ->
      let inds = OList.combine inds indsn in
      let indsn = OList.rev indsn in (* Backwards ordering w.r.t. Fun *)
      let+ inds = List.map (fun ((i, ({ mind_user_lc; mind_consnames; _ } as ib)), n) ->
          register_inductive (m, i) n >>
          let gen_constr_typ typ = match mind_record with
            | NotRecord | FakeRecord -> gen_constr typ
            | PrimRecord _ -> gen_primitive_constructor (m, i) (OList.length mind_params_ctxt) typ in
          let constructs = OList.mapi (fun j x -> j, x) @@
            OList.combine (Array.to_list mind_user_lc) (Array.to_list mind_consnames) in
          let+ children =
            let* cstrs = List.map (fun (j, (typ, id)) ->
                with_relatives indsn @@
                let* typ = gen_constr_typ typ in
                let* n = mk_node (mk_definition (Construct ((m, i), j + 1))) [ConstructTerm, typ] in
                let+ () = register_constructor ((m, i), j + 1) n in
                IndConstruct, n) constructs in
            let univs = Declareops.inductive_polymorphic_context mb in
            let inst = Univ.make_abstract_instance univs in
            let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) env in
            let typ = Inductive.type_of_inductive env ((mb, ib), inst) in
            let+ n = gen_constr typ in
            (IndType, n)::cstrs in
          mk_definition (Ind (m, i)), children
        ) inds in
      (), inds
  and gen_inductive ((m, _) as i) : G.node t =
    follow_def (mk_definition @@ Ind i)
      (gen_mutinductive_helper m >>
       let+ n = find_inductive i in
       match n with
       | Some inn -> inn
       | None -> CErrors.anomaly (Pp.str "Inductive generation problem"))
  and gen_constructor (((m, _), _) as c) : G.node t =
    follow_def (mk_definition @@ Construct c)
      (gen_mutinductive_helper m >>
       let+ n = find_constructor c in
       match n with
       | Some cn -> cn
       | None -> CErrors.anomaly (Pp.str "Inductive generation problem"))
  and gen_projection p =
    follow_def (mk_definition @@ Proj (Projection.repr p))
      (gen_mutinductive_helper (Projection.mind p) >>
       let+ n = find_projection (Projection.repr p) in
       match n with
       | Some cn -> cn
       | None -> CErrors.anomaly (Pp.str "Inductive generation problem"))
  and gen_constr c = gen_kind_of_term @@ Constr.kind c
  and gen_kind_of_term = function
    | Rel i ->
      let* ino = lookup_relative i in
      mk_node Rel [RelPointer, ino]
    | Var id ->
      let* ino = lookup_named id in
      mk_node Var [VarPointer, ino]
    | Meta i ->
      CErrors.anomaly (Pp.str "Unexpected meta")
    | Evar (ev, substs) -> (* TODO: Add type and proper substitution list *)
      let* substs = match Array.to_list substs with
       | [] -> return []
       | head::substs ->
         let* head = gen_constr head in
         let* head = mk_node (* EvarSubstPointer *) EvarSubst [EvarSubstValue, head] in
         let+ _, substs = List.fold_left (fun (prev, acc) subst ->
             let* subst = gen_constr subst in
             let+ subst = mk_node EvarSubst [EvarSubstValue, subst; EvarSubstOrder, prev] in
             subst, (EvarSubstPointer, prev)::acc)
           (head, [EvarSubstPointer, head]) substs in
         OList.rev substs in
      mk_node (Evar (Evar.repr ev)) substs
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
      let* fn = mk_node AppFun [AppFunValue, f] in
      let* (_, args) = List.fold_left (fun (prev, acc) arg ->
          let* arg = gen_constr arg in
          let+ arg = mk_node AppArg [AppArgValue, arg; AppArgOrder, prev] in
          arg, (AppArgPointer, arg)::acc)
          (fn, [AppFunPointer, fn]) @@ Array.to_list args in
      mk_node App (OList.rev args)
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
        let combined = OList.combine funs @@ OList.combine (Array.to_list ids) @@ OList.combine (Array.to_list typs) (Array.to_list terms) in
        let+ children = List.map (fun (fn, (id, (typ, term))) ->
            let* typ = gen_constr typ in
            let+ term = with_relatives (OList.rev funs) @@ gen_constr term in
            (FixFun id.binder_name), [FixFunType, typ; FixFunTerm, term])
            combined in
        funs, children in
      mk_node Fix @@ (FixReturn, (OList.nth funs ret))::(OList.map (fun f -> FixMutual, f) funs)
    | CoFix (ret, (ids, typs, terms)) ->
      let* funs = with_delayed_nodes (Array.length ids) @@ fun funs ->
        let combined = OList.combine funs @@ OList.combine (Array.to_list ids) @@ OList.combine (Array.to_list typs) (Array.to_list terms) in
        let+ children = List.map (fun (fn, (id, (typ, term))) ->
            let* typ = gen_constr typ in
            let+ term = with_relatives (OList.rev funs) @@ gen_constr term in
            (CoFixFun id.binder_name), [CoFixFunType, typ; CoFixFunTerm, term])
            combined in
        funs, children in
      mk_node CoFix @@ (CoFixReturn, (OList.nth funs ret))::(OList.map (fun f -> CoFixMutual, f) funs)
    | Proj (p, term) ->
      let* fn = gen_projection p in
      let* fn = mk_node AppFun [AppFunValue, fn] in
      let* term = gen_constr term in
      let* arg = mk_node AppArg [AppArgValue, term; AppArgOrder, fn] in
      mk_node App [AppFunPointer, fn; AppArgPointer, arg]
    | Int n ->
      mk_node (Int n) []
    | Float f ->
      mk_node (Float f) []

  let with_named_context ctx m = with_named_context gen_constr ctx m

  let gen_globref = function
    | GlobRef.VarRef _ -> CErrors.anomaly (Pp.str "not handled yet")
    | GlobRef.ConstRef c -> gen_const c
    | GlobRef.IndRef i -> gen_inductive i
    | GlobRef.ConstructRef c -> gen_constructor c

  let with_envs f env env_extra x = with_env env (with_env_extra env_extra (f x))

  let gen_proof_state env_extra =
    Proofview.Goal.enter_one (fun g ->
        let concl = Proofview.Goal.concl g in
        let hyps = Proofview.Goal.hyps g in
        let sigma = Proofview.Goal.sigma g in
        let hyps = OList.map (map_named (EConstr.to_constr sigma)) hyps in
        let env = Proofview.Goal.env g in
        Proofview.tclUNIT @@
        with_env env @@ with_env_extra env_extra @@
        let* hyps, concl = with_named_context hyps @@ gen_constr (EConstr.to_constr sigma concl) in
        mk_node Root ((ContextSubject, concl)::hyps))

  let gen_const = with_envs gen_const
  let gen_mutinductive_helper = with_envs gen_mutinductive_helper
  let gen_inductive = with_envs gen_inductive
  let gen_constructor = with_envs gen_constructor
  let gen_projection = with_envs gen_projection
  let gen_constr = with_envs gen_constr
  let with_named_context env env_extra ps = with_envs (with_named_context ps) env env_extra
  let gen_globref = with_envs gen_globref

end
