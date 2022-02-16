open Tactician_ltac1_record_plugin
open Labelled_graph_def
open Names
open Declarations
open Context
open Ltac_plugin
open Tacexpr

module ProjMap = CMap.Make(Projection.Repr)

module type CICGraphMonadType = sig

  include Monad.Def
  type node
  type node_label
  type edge_label
  type 'a repr_t

  (** Basic drawing primitives. More high-level primitives are defined below and should be preferred *)
  val mk_node : node_label -> node t
  val mk_edge : edge_label -> source:node -> target:node -> unit t
  val with_delayed_node : (node -> ('a * node_label) t) -> 'a t

  (** Registering and lookup of definitions *)
  val register_constant : Constant.t -> node -> unit t
  val find_constant : Constant.t -> node option t

  val register_inductive : inductive -> node -> unit t
  val find_inductive : inductive -> node option t

  val register_constructor : constructor -> node -> unit t
  val find_constructor : constructor -> node option t

  val register_projection : Projection.Repr.t -> node -> unit t
  val find_projection : Projection.Repr.t -> node option t

  (** Managing the local context and the focus *)
  val lookup_relative    : int -> node t
  val lookup_named       : Id.t -> node t
  val lookup_named_map   : node Id.Map.t t
  val lookup_focus       : node t
  val lookup_follow_defs : bool t
  val lookup_proofs_map : (((Constr.t, Constr.t) Named.Declaration.pt list * Constr.t) list * glob_tactic_expr) list Cmap.t t

  val get_named_context : node Id.Map.t t

  val with_relative       : node -> 'a t -> 'a t
  val with_relatives      : node list -> 'a t -> 'a t
  val with_named          : Id.t -> node -> 'a t -> 'a t
  val with_focus          : node -> 'a t -> 'a t
  val with_initial_focus  : 'a t -> 'a t
  val with_empty_contexts : 'a t -> 'a t

  (** High level drawing commands that draw from a given focus *)

  (** [draw_toward_delayed el f] creates a new node [n], draws an edge from the focus (if any) to [n] with [el] as edge label,
      and runs the computation [f n] in a context where the focus is kept to the original. Finally, the output of [f n] determines the
      node label of [n] *)
  val draw_toward_delayed : edge_label -> (node -> ('a * node_label) t) -> 'a t

  (** [move_toward_delayed el f] creates a new node [n], draws an edge from the focus (if any) to [n] with [el] as edge label,
      and runs the computation [f n] in a context where the focus is set to [n]. Finally, the output of [f n] determines the
      node label of [n] *)
  val move_toward_delayed : edge_label -> (node -> ('a * node_label) t) -> 'a t

  (** Variations of the three functions above *)
  val draw_toward                 : edge_label -> node -> unit t
  val draw_toward_new             : edge_label -> node_label -> node t
  val draw_toward_new_unit        : edge_label -> node_label -> unit t

  val move_toward                 : edge_label -> node -> 'a t -> 'a t
  val move_toward_new             : edge_label -> node_label -> unit t -> node t
  val move_toward_new_unit        : edge_label -> node_label -> unit t -> unit t
  val move_toward_delayed_unit    : edge_label -> (node -> node_label t) -> node t
  val move_toward_delayed_ignore  : edge_label -> (node -> node_label t) -> unit t
  val move_toward_new_with_parent : edge_label -> node_label -> (node -> 'a t) -> 'a t

  type definitions =
    { constants            : node Cmap.t
    ; inductives           : node Indmap.t
    ; constructors         : node Constrmap.t
    ; projections          : node ProjMap.t }

  val run :
    initial_focus:node_label ->
    follow_defs:advanced_flag ->
    known_definitions:definitions ->
    (((Constr.t, Constr.t) Named.Declaration.pt list * Constr.t) list * glob_tactic_expr) list Cmap.t ->
    'a t -> (definitions * 'a) repr_t
  val run_empty :
    initial_focus:node_label ->
    follow_defs:advanced_flag ->
    (((Constr.t, Constr.t) Named.Declaration.pt list * Constr.t) list * glob_tactic_expr) list Cmap.t ->
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
    { relative    : node list
    ; named       : node Id.Map.t
    ; focus       : node
    ; follow_defs : bool
    ; proofs_map : (((Constr.t, Constr.t) Named.Declaration.pt list * Constr.t) list * glob_tactic_expr) list Cmap.t
    ; initial_focus : node }
  type directed_edge =
    { source : node
    ; target : node
    ; label  : edge_label }

  module M = Monad_util.ReaderStateMonadT
      (G)
      (struct type r = context end)
      (struct type s = definitions end)
  module OList = List
  open M
  include Monad_util.WithMonadNotations(M)

  (** Lifting of basic drawing primitives *)
  let mk_node nl = M.lift @@ mk_node nl
  let mk_edge el ~source ~target = M.lift @@ mk_edge el ~source ~target
  let with_delayed_node f = unrun @@ fun c s ->
    with_delayed_node (fun n ->
        G.map (fun (s, (a, nl)) -> (s, a), nl) (run (f n) c s))

  let get_named_context =
    let+ context = ask in
    context.named

  (** Registering and lookup of definitions *)

  let update_error x = function
    | None -> Some x
    | Some _ -> CErrors.anomaly (Pp.str "Map update attempt while key already existed")

  let register_constant c n =
    let* ({ constants; _ } as s) = get in
    put { s with constants = Cmap.update c (update_error n) constants }
  let find_constant c =
    let+ { constants; _ } = get in
    Cmap.find_opt c constants

  let register_inductive i n =
    let* ({ inductives; _ } as s) = get in
    put { s with inductives = Indmap.update i (update_error n) inductives }
  let find_inductive i =
    let+ { inductives; _ } = get in
    Indmap.find_opt i inductives

  let register_constructor c n =
    let* ({ constructors; _ } as s) = get in
    put { s with constructors = Constrmap.update c (update_error n) constructors }
  let find_constructor c =
    let+ { constructors; _ } = get in
    Constrmap.find_opt c constructors

  let register_projection p n =
    let* ({ projections; _ } as s) = get in
    put { s with projections = ProjMap.update p (update_error n) projections }
  let find_projection p =
    let+ { projections; _ } = get in
    ProjMap.find_opt p projections

  (** Managing the local context and the focus *)
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
  let lookup_focus =
    let+ { focus; _ } = ask in focus
  let lookup_follow_defs =
    let+ { follow_defs; _ } = ask in
    follow_defs
  let lookup_proofs_map =
    let+ { proofs_map; _ } = ask in
    proofs_map

  let with_relative n =
    local (fun ({ relative; _ } as c) -> { c with relative = n::relative })
  let with_var id var =
    local (fun ({ named; _ } as c) -> { c with named = Id.Map.add id var named })
  let focus =
    let+ { focus; _ } = ask in focus
  let with_focus focus =
    local (fun c -> { c with focus })
  let with_initial_focus m =
    local (fun c -> { c with focus = c.initial_focus }) m
  let with_relatives ns =
    local (fun ({ relative; _ } as c) ->
        let relative = OList.fold_left (fun ctx n -> n::ctx) ns relative in (* Funs are added backwards *)
        { c with relative })
  let with_named id n =
    local (fun ({ named; _ } as c) -> { c with named = Id.Map.update id (update_error n) named })
  let with_focus focus =
    local (fun c -> { c with focus })
  let with_empty_contexts m =
    local (fun c -> { c with named = Id.Map.empty; relative = [] }) m

  (** High level drawing commands that draw from a given focus *)

  let draw_toward et target =
    let* { focus; _ } = ask in
    mk_edge et ~source:focus ~target
  let move_toward et target m =
    draw_toward et target >>
    local (fun c -> { c with focus = target }) m
  let move_toward_new_with_parent et nt m =
    let* n = mk_node nt in
    move_toward et n (m n)
  let move_toward_new et nt m =
    move_toward_new_with_parent et nt (fun i -> m >> return i)
  let move_toward_new_unit et nt m =
    move_toward_new_with_parent et nt (fun i -> m)
  let draw_toward_new et nt =
    move_toward_new et nt (return ())
  let draw_toward_new_unit et nt =
    move_toward_new_unit et nt (return ())

  let draw_toward_delayed et f =
    with_delayed_node @@
    fun n -> draw_toward et n >> f n

  let move_toward_delayed et f =
    with_delayed_node @@
    fun n -> move_toward et n (f n)

  let move_toward_delayed_unit et f =
    with_delayed_node @@
    fun n -> move_toward et n @@
    let+ nt = f n in
    n, nt

  let move_toward_delayed_ignore et f =
    with_delayed_node @@
    fun n -> move_toward et n @@
    let+ nt = f n in
    (), nt

  let run ~initial_focus ~follow_defs ~known_definitions proofs_map m =
    (* The initial focus node is superfluous, but who cares *)
    G.run @@ G.(>>=) (G.mk_node initial_focus) (fun focus ->
        let context =
          { relative = []; named = Id.Map.empty; focus; follow_defs; proofs_map; initial_focus = focus } in
        run m context known_definitions)
  let run_empty ~initial_focus ~follow_defs proofs_map m =
    let known_definitions =
      { constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = ProjMap.empty} in
    run ~initial_focus ~follow_defs ~known_definitions proofs_map m
end

module GraphBuilder
    (G : sig
       type node'
       include CICGraphMonadType
         with type node = node'
          and type edge_label = edge_type
          and type node_label = node' node_type
    end) = struct

  module OList = List
  open G
  open Monad.Make(G)
  open Monad_util.WithMonadNotations(G)

  let update_error x = function
    | None -> Some x
    | Some _ -> CErrors.anomaly (Pp.str "Map update attempt while key already existed")

  let const x _ = x
  let ignore' m = map ignore m

  let cached_gen_const et c gen : G.node t =
    let* n = find_constant c in
    match n with
    | Some c -> draw_toward et c >> return c
    | None ->
      let* n = with_empty_contexts gen in
      let+ () = register_constant c n in
      n

  let with_named_context gen_constr c (m : 'a t) =
    Named.fold_inside (fun m d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          let* var = move_toward_new ContextElem (ContextAssum id.binder_name) @@
            gen_constr ContextDefType typ in
          with_named id.binder_name var m
        | Named.Declaration.LocalDef (id, term, typ) ->
          let* var = move_toward_new ContextElem (ContextDef id.binder_name)
            (gen_constr ContextDefType typ >>
             gen_constr ContextDefTerm term) in
          with_named id.binder_name var m)
      c ~init:m

  let mk_definition def_type = Definition { previous = []; def_type }

  let dirpath = Global.current_dirpath ()

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

  let rec gen_const et c : G.node t =
    (* Only process canonical constants *)
    let c = Constant.make1 (Constant.canonical c) in
    cached_gen_const et c @@
    let* proofs = lookup_proofs_map in
    let proof = Cmap.find_opt c proofs in
    let gen_const d =
      move_toward_new et (mk_definition d) @@
      let { const_hyps; const_body; const_type; _ } = Global.lookup_constant c in
      with_named_context gen_constr const_hyps
        (gen_constr ConstType const_type >>
         match const_body with
         | Undef _ -> draw_toward_new_unit ConstUndef ConstEmpty
         | Def c -> gen_constr ConstDef @@ Mod_subst.force_constr c
         | OpaqueDef c ->
           let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Global.opaque_tables ()) c in
           gen_constr ConstOpaqueDef c
         | Primitive p -> draw_toward_new_unit ConstPrimitive @@ Primitive p) in
    match proof with
    | None -> gen_const (ManualConst c)
    | Some proof ->
      let proof = OList.concat @@ OList.map (fun (pss, tac) -> OList.map (fun ps -> ps, tac) pss) proof in
      let* proof = List.map gen_tactical_proof proof in
      gen_const (TacticalConstant (c, proof))
  and gen_const2 et (c : constant) =
    let+ _ = gen_const et c in ()
  and gen_proof_state (hyps, concl) =
    with_named_context gen_constr hyps @@
      (gen_constr ContextSubject concl >>
       lookup_named_map)
  and gen_tactical_proof (ps, tac) =
    let* root = mk_node Root in
    let* context_map = with_focus root @@ gen_proof_state ps in
    let tac_orig = Tactic_name_remove.tactic_name_remove tac in
    let tac = Tactic_normalize.tactic_normalize @@ Tactic_normalize.tactic_strict tac_orig in
    let args, interm_tactic = Tactic_one_variable.tactic_one_variable tac in
    let tac = Extreme_tactic_normalize.tactic_normalize tac in
    let context = Id.Map.bindings context_map in
    let context_range = OList.map (fun (_, n) -> n) context in
    let warn_arg id =
      Feedback.msg_warning Pp.(str "Unknown tactical argument: " ++ Id.print id ++ str " in tactic " ++
                               Pptactic.pr_glob_tactic (Global.env ()) tac_orig (* ++ str " in context\n" ++ *)
                               (* prlist_with_sep (fun () -> str "\n") (fun (id, node) -> Id.print id ++ str " : ") context *)) in
    let check_default id = function
      | None -> warn_arg id; None
      | x -> x in
    let+ arguments =
      let open Tactic_one_variable in
      List.map (function
          | TVar id ->
            return @@ check_default id @@
            Option.map snd @@ OList.find_opt (fun (id2, n) -> Id.equal id id2) context
          | TRef c ->
            (* TODO: We are generating extra edges here, improve *)
            (match c with
              | GlobRef.VarRef _ -> assert false
              | GlobRef.ConstRef c ->
                let+ c = with_initial_focus @@ gen_const ContextElem c in
                Some c
              | GlobRef.IndRef i ->
                let+ c = with_initial_focus @@ gen_inductive ContextElem i in
                Some c
              | GlobRef.ConstructRef c ->
                let+ c = with_initial_focus @@ gen_constructor ContextElem c in
                Some c
            )
          | TOther -> return None
        ) args in
    let ps_string = proof_state_to_string_safe ps (Global.env ()) Evd.empty in
    { tactic = tac_orig; base_tactic = tac; interm_tactic
    ; tactic_hash = Hashtbl.hash_param 255 255 tac
    ; arguments = arguments (* @ constants *)
    ; root; context = context_range; ps_string }
  and gen_primitive_constructor et ind proj_npars typ =
    let relctx, sort = Term.decompose_prod_assum typ in
    let real_arity = Rel.nhyps @@ CList.skipn proj_npars @@ OList.rev relctx in
    let generator = snd @@ CList.fold_left (fun (reli, m) d ->
        match d with
        | Rel.Declaration.LocalAssum ({ binder_name = id; _ }, typ) ->
          reli - 1, fun et -> move_toward_new_unit et (Prod id)
            (let* prod = lookup_focus in
             (match id with
              | Name id when reli >= 0 ->
                let proj = Projection.Repr.make
                    ind ~proj_npars ~proj_arg:reli @@ Label.of_id id in
                let* nproj = mk_node (mk_definition (Proj proj)) in
                register_projection proj nproj >>
                with_focus nproj @@ draw_toward ProjTerm prod
              | _ -> return ()) >>
             gen_constr ProdType typ >>
             with_relative prod @@ m ProdTerm)
        | Rel.Declaration.LocalDef ({ binder_name = id; _ }, term, typ) ->
          reli, fun et -> move_toward_new_unit et (LetIn id)
            (gen_constr LetInType typ >>
             gen_constr LetInDef term >>
             let* letin = lookup_focus in
             with_relative letin @@ m LetInDef))
        (real_arity - 1, (fun et -> gen_constr et sort)) relctx in
    generator et
  and gen_mutinductive_helper m =
    (* Only process canonical inductives *)
    let m = MutInd.make1 (MutInd.canonical m) in
    let* c = find_inductive (m, 0) in
    match c with
    | Some mn -> return ()
    | None ->
      with_empty_contexts @@
      let ({ mind_hyps; mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
        Global.lookup_mind m in
      with_named_context gen_constr mind_hyps @@
      let inds = OList.mapi (fun i ind -> i, ind) (Array.to_list mind_packets) in
      let* inds = List.map (fun (i, ind) ->
          let* n = mk_node @@ mk_definition (Ind (m, i)) in
          register_inductive (m, i) n >> return (i, ind, n)) inds in
      let indsn = OList.rev @@ OList.map (fun (_, _, n) -> n) inds in (* Backwards ordering w.r.t. Fun *)
      List.iter (fun (i, ({ mind_user_lc; mind_consnames; _ } as ib), n) ->
          let gen_constr_typ et typ = match mind_record with
            | NotRecord | FakeRecord -> gen_constr et typ
            | PrimRecord _ -> gen_primitive_constructor et (m, i) (OList.length mind_params_ctxt) typ in
          let constructs = OList.mapi (fun j x -> j, x) @@
            OList.combine (Array.to_list mind_user_lc) (Array.to_list mind_consnames) in
          with_focus n
            (List.iter (fun (j, (typ, id)) ->
                 with_relatives indsn @@ move_toward_new IndConstruct (mk_definition (Construct ((m, i), j + 1))) @@
                 gen_constr_typ ConstructTerm typ >>=
                 register_constructor ((m, i), j + 1)) constructs >>
             let univs = Declareops.inductive_polymorphic_context mb in
             let inst = Univ.make_abstract_instance univs in
             let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) (Global.env ()) in
             let typ = Inductive.type_of_inductive env ((mb, ib), inst) in
             gen_constr IndType typ)) inds
  and gen_inductive et ((m, _) as i) : G.node t =
    gen_mutinductive_helper m >>
    let* n = find_inductive i in
    match n with
    | Some inn -> draw_toward et inn >> return inn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and gen_inductive2 et i =
    let+ _ = gen_inductive et i in ()
  and gen_constructor et (((m, _), _) as c) : G.node t =
    gen_mutinductive_helper m >>
    let* n = find_constructor c in
    match n with
    | Some cn -> draw_toward et cn >> return cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and gen_constructor2 et c =
    let+ _ = gen_constructor et c in ()
  and gen_projection et p =
    gen_mutinductive_helper (Projection.mind p) >>
    let* n = find_projection (Projection.repr p) in
    match n with
    | Some cn -> draw_toward et cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and follow_def et nt def =
    let* follow_defs = lookup_follow_defs in
    if follow_defs then def else draw_toward_new_unit et nt
  and gen_constr et c = gen_kind_of_term et @@ Constr.kind c
  and gen_kind_of_term et = function
    | Rel i ->
      let* ino = lookup_relative i in
      move_toward_new_unit et Rel @@ draw_toward RelPointer ino
    | Var id ->
      move_toward_new_unit et Var (lookup_named id >>= draw_toward VarPointer)
    | Meta i ->
      CErrors.anomaly (Pp.str "Unexpected meta")
    | Evar (ev, substs) -> (* TODO: Add type and proper substitution list *)
      move_toward_new_unit et (Evar (Evar.repr ev))
        (match Array.to_list substs with
         | [] -> return ()
         | h::substs ->
           let* head = move_toward_new EvarSubstPointer EvarSubst @@ gen_constr EvarSubstValue h in
           ignore' @@ List.fold_left (fun prev b ->
               let* curr = move_toward_new EvarSubstPointer EvarSubst @@ gen_constr EvarSubstValue b in
               map (const curr) @@ mk_edge EvarSubstOrder ~source:prev ~target:curr)
             head substs)
    | Sort s ->
        (match s with
         | Sorts.SProp -> draw_toward_new_unit et SortSProp
         | Sorts.Prop -> draw_toward_new_unit et SortProp
         | Sorts.Set -> draw_toward_new_unit et SortSet
         | Sorts.Type _ -> draw_toward_new_unit et SortType)
    | Cast (term, kind, typ) ->
      move_toward_new_unit et Cast
        (gen_constr CastTerm term >>
         gen_constr CastType typ)
    | Prod (id, bi, conc) ->
      move_toward_new_unit et (Prod id.binder_name)
        (gen_constr ProdType bi >>
         let* prod = lookup_focus in
         with_relative prod @@ gen_constr ProdTerm conc)
    | Lambda (id, typ, term) ->
      move_toward_new_unit et (Lambda id.binder_name)
        (gen_constr LambdaType typ >>
         let* lambda = lookup_focus in
         with_relative lambda @@ gen_constr LambdaTerm term)
    | LetIn (id, def, typ, term) ->
      move_toward_new_unit et (LetIn id.binder_name)
        (gen_constr LetInDef def >>
         gen_constr LetInType typ >>
         let* letin = lookup_focus in
         with_relative letin @@ gen_constr LetInTerm term)
    | App (f, args) ->
      move_toward_new_unit et App @@
      let* fn = move_toward_new AppFunPointer AppFun @@ gen_constr AppFunValue f in
      ignore' @@ List.fold_left (fun prev b ->
          let* curr = move_toward_new AppArgPointer AppArg @@ gen_constr AppArgValue b in
          map (const curr) @@ mk_edge AppArgOrder ~source:prev ~target:curr)
        fn (Array.to_list args)
    | Const (c, u) ->
      follow_def et (mk_definition (ManualConst c)) @@ gen_const2 et c
    | Ind (i, u) ->
      follow_def et (mk_definition (Ind i)) @@ gen_inductive2 et i
    | Construct (c, u) ->
      follow_def et (mk_definition (Construct c)) @@ gen_constructor2 et c
    | Case (i, ret, term, branches) ->
      move_toward_new_unit et Case
        (follow_def CaseInd (mk_definition (Ind i.ci_ind)) @@ gen_inductive2 CaseInd i.ci_ind >>
         gen_constr CaseReturn ret >>
         gen_constr CaseTerm term >>
         List.iter (fun (c, branch) ->
             move_toward_new_unit CaseBranchPointer CaseBranch
               (follow_def CBConstruct (mk_definition (Construct (i.ci_ind, c + 1))) @@ gen_constructor2 CBConstruct (i.ci_ind, c + 1) >>
                gen_constr CBTerm branch))
           (OList.mapi (fun i x -> i, x) @@ Array.to_list branches))
    | Fix ((offset, ret), (ids, typs, terms)) ->
      move_toward_new_unit et Fix @@
      let* funs = List.map (fun id -> mk_node @@ FixFun id.binder_name) (Array.to_list ids) in
      let combined = OList.combine funs (OList.combine (Array.to_list typs) (Array.to_list terms)) in
      List.iter (fun (fn, (typ, term)) ->
          move_toward FixMutual fn
            (gen_constr FixFunType typ >>
             with_relatives funs @@ gen_constr FixFunTerm term))
        combined >>
      draw_toward FixReturn (OList.nth funs ret)
    | CoFix (ret, (ids, typs, terms)) ->
      move_toward_new_unit et CoFix @@
      let* funs = List.map (fun id -> mk_node @@ CoFixFun id.binder_name) (Array.to_list ids) in
      let combined = OList.combine funs (OList.combine (Array.to_list typs) (Array.to_list terms)) in
      List.iter (fun (fn, (typ, term)) ->
          move_toward CoFixMutual fn
            (gen_constr CoFixFunType typ >>
             with_relatives funs @@ gen_constr CoFixFunTerm term))
        combined >>
      draw_toward CoFixReturn (OList.nth funs ret)
    | Proj (p, term) ->
      move_toward_new_unit et App @@
      let* fn = move_toward_new AppFunPointer AppFun @@ follow_def AppFunValue (mk_definition (Proj (Projection.repr p))) @@ gen_projection AppFunValue p in
      let* arg = move_toward_new AppArgPointer AppArg @@ gen_constr AppArgValue term in
      mk_edge AppArgOrder ~source:fn ~target:arg
    | Int n ->
      draw_toward_new_unit et @@ Int n
    | Float f ->
      draw_toward_new_unit et @@ Float f

  let with_named_context ctx m = with_named_context gen_constr ctx m

  let map_named f = function
    | Named.Declaration.LocalAssum (id, ty) ->
      let ty' = f ty in Named.Declaration.LocalAssum (id, ty')
    | Named.Declaration.LocalDef (id, v, ty) ->
      let v' = f v in
      let ty' = f ty in Named.Declaration.LocalDef (id, v', ty')
  let gen_proof_state =
    Proofview.Goal.enter_one (fun g ->
        let concl = Proofview.Goal.concl g in
        let hyps = Proofview.Goal.hyps g in
        let sigma = Proofview.Goal.sigma g in
        let hyps = OList.map (map_named (EConstr.to_constr sigma)) hyps in
        let ret = with_named_context hyps @@ gen_constr ContextSubject (EConstr.to_constr sigma concl)
      in Proofview.tclUNIT ret)

  let gen_globref = function
    | GlobRef.VarRef _ -> CErrors.anomaly (Pp.str "not handled yet")
    | GlobRef.ConstRef c -> gen_const2 ContextSubject c
    | GlobRef.IndRef i -> gen_inductive2 ContextSubject i
    | GlobRef.ConstructRef c -> gen_constructor2 ContextSubject c
end
