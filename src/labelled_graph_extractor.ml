open Tactician_ltac1_record_plugin
open Labelled_graph_def
open Names
open Declarations
open Context
open Ltac_plugin
open Tacexpr

module ReaderStateMonad (R : sig type r and s end) = struct
  open R
  type 'a t = (s * r) -> (s * 'a)
  let return x = fun (s, _) -> s,x
  let (>>=) x f = fun (s, r) -> let s, x = x (s, r) in f x (s, r)
  let (>>) x y = fun (s, r) -> let s, () = x (s, r) in y (s, r)
  let map f x = fun sr -> let s, x = x sr in s, f x

  let put s = fun _ -> s, ()
  let get = fun (s, _) -> s, s

  let ask sr = sr
  let local f x = fun (s, r) -> x (s, f r)
end

module GraphBuilder(G : Graph) = struct

  module ProjMap = CMap.Make(Projection.Repr)

  type state =
    { graph         : G.t
    ; constants     : G.node Cmap.t
    ; inductives    : G.node Indmap.t
    ; constructors  : G.node Constrmap.t
    ; projections   : G.node ProjMap.t }

  type context =
    { relative    : G.node list
    ; named       : G.node Id.Map.t
    ; focus       : G.node
    ; follow_defs : bool
    ; proofs_map : (((Constr.t, Constr.t) Named.Declaration.pt list * Constr.t) list * glob_tactic_expr) list Cmap.t
    ; initial_focus : G.node }

  module M = ReaderStateMonad(struct type s = state and r = context end)
  open M
  open Tactician_util.WithMonadNotations(M)
  module OList = List
  open Monad.Make(M)

  let run_empty follow_defs proofs_map m =
    let focus, graph = G.mk_node G.empty Root in
    let state =
      { graph
      ; constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = ProjMap.empty} in
    let context = { relative = []; named = Id.Map.empty; focus; follow_defs; proofs_map; initial_focus = focus } in
    m (state, context)

  let const x _ = x
  let ignore' m = map ignore m

  let mk_node nt =
    let* ({ graph; _ } as g) = get in
    let n, graph = G.mk_node graph nt in
    let+ () = put { g with graph } in
    n
  let mk_edge sort source target =
    let* { graph; _ } as g = get in
    put { g with graph = G.mk_edge graph sort ~source ~target }
  let relative_lookup i =
    let+ { relative; _ } = ask in
    let rec find ctx i = match ctx, i with
      | node::_, 1 -> node
      | _::ctx, n -> find ctx (n - 1)
      | [], _ -> CErrors.anomaly (Pp.str "Invalid relative context") in
    find relative i
  let named_lookup id =
    let+ { named; _ } = ask in
    Id.Map.find id named
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
  let with_reset m =
    local (fun c -> { c with named = Id.Map.empty; relative = [] }) m

  let draw_toward et n =
    let* { focus; _ } = ask in
    mk_edge et focus n
  let move_toward et n m =
    draw_toward et n >>
    local (fun c -> { c with focus = n }) m
  let move_toward_new et nt m =
    let* n = mk_node nt in
    move_toward et n m >> return n
  let move_toward_new' et nt m = ignore' @@ move_toward_new et nt m
  let draw_toward_new et nt =
    move_toward_new et nt (return ())
  let draw_toward_new' et nt = move_toward_new' et nt (return ())

  let register_ind i n =
    let* ({ inductives; _ } as s) = get in
    put { s with inductives = Indmap.add i n inductives }
  let register_construct c n =
    let* ({ constructors; _ } as s) = get in
    put { s with constructors = Constrmap.add c n constructors }
  let register_projection p n =
    let* ({ projections; _ } as s) = get in
    put { s with projections = ProjMap.add p n projections }

  let cached_gen_const et c gen : G.node t =
    let* { constants; _ } = get in
    match Cmap.find_opt c constants with
    | Some c -> draw_toward et c >> return c
    | None ->
      let* n = with_reset gen in
      let* ({ constants; _ } as s) = get in
      let+ _ = put { s with constants = Cmap.add c n constants } in
      n

  let with_named_context gen_constr c (m : 'a t) =
    Named.fold_inside (fun m d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          let* var = move_toward_new ContextElem (ContextAssum id.binder_name) @@
            gen_constr ContextDefType typ in
          with_var id.binder_name var m
        | Named.Declaration.LocalDef (id, term, typ) ->
          let* var = move_toward_new ContextElem (ContextDef id.binder_name)
            (gen_constr ContextDefType typ >>
             gen_constr ContextDefTerm term) in
          with_var id.binder_name var m)
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
    let* r = ask in
    let proof = Cmap.find_opt c r.proofs_map in
    let gen_const d =
      move_toward_new et (mk_definition d) @@
      let { const_hyps; const_body; const_type; _ } = Global.lookup_constant c in
      with_named_context gen_constr const_hyps
        (gen_constr ConstType const_type >>
         match const_body with
         | Undef _ -> draw_toward_new' ConstUndef ConstEmpty
         | Def c -> gen_constr ConstDef @@ Mod_subst.force_constr c
         | OpaqueDef c ->
           let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Global.opaque_tables ()) c in
           gen_constr ConstOpaqueDef c
         | Primitive p -> draw_toward_new' ConstPrimitive @@ Primitive p) in
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
       map (fun c -> c.named) ask)
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
          reli - 1, fun et -> move_toward_new' et (Prod id)
            (let* prod = focus in
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
          reli, fun et -> move_toward_new' et (LetIn id)
            (gen_constr LetInType typ >>
             gen_constr LetInDef term >>
             let* letin = focus in
             with_relative letin @@ m LetInDef))
        (real_arity - 1, (fun et -> gen_constr et sort)) relctx in
    generator et
  and gen_mutinductive_helper m =
    (* Only process canonical inductives *)
    let m = MutInd.make1 (MutInd.canonical m) in
    let* { inductives; _ } = get in
    match Indmap.find_opt (m, 0) inductives with
    | Some mn -> return ()
    | None ->
      with_reset @@
      let ({ mind_hyps; mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
        Global.lookup_mind m in
      with_named_context gen_constr mind_hyps @@
      let inds = OList.mapi (fun i ind -> i, ind) (Array.to_list mind_packets) in
      let* inds = List.map (fun (i, ind) ->
          let* n = mk_node @@ mk_definition (Ind (m, i)) in
          register_ind (m, i) n >> return (i, ind, n)) inds in
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
                 register_construct ((m, i), j + 1)) constructs >>
             let univs = Declareops.inductive_polymorphic_context mb in
             let inst = Univ.make_abstract_instance univs in
             let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) (Global.env ()) in
             let typ = Inductive.type_of_inductive env ((mb, ib), inst) in
             gen_constr IndType typ)) inds
  and gen_inductive et ((m, _) as i) : G.node t =
    gen_mutinductive_helper m >>
    let* { inductives; _ } = get in
    match Indmap.find_opt i inductives with
    | Some inn -> draw_toward et inn >> return inn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and gen_inductive2 et i =
    let+ _ = gen_inductive et i in ()
  and gen_constructor et (((m, _), _) as c) : G.node t =
    gen_mutinductive_helper m >>
    let* { constructors; _ } = get in
    match Constrmap.find_opt c constructors with
    | Some cn -> draw_toward et cn >> return cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and gen_constructor2 et c =
    let+ _ = gen_constructor et c in ()
  and gen_projection et p =
    gen_mutinductive_helper (Projection.mind p) >>
    let* { projections; _ } = get in
    match ProjMap.find_opt (Projection.repr p) projections with
    | Some cn -> draw_toward et cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and follow_def et nt def =
    let* ({ follow_defs; _ }) = ask in
    if follow_defs then def else draw_toward_new' et nt
  and gen_constr et c = gen_kind_of_term et @@ Constr.kind c
  and gen_kind_of_term et = function
    | Rel i ->
      let* ino = relative_lookup i in
      move_toward_new' et Rel @@ draw_toward RelPointer ino
    | Var id ->
      move_toward_new' et Var (named_lookup id >>= draw_toward VarPointer)
    | Meta i ->
      CErrors.anomaly (Pp.str "Unexpected meta")
    | Evar (ev, substs) -> (* TODO: Add type and proper substitution list *)
      move_toward_new' et (Evar (Evar.repr ev))
        (match Array.to_list substs with
         | [] -> return ()
         | h::substs ->
           let* head = move_toward_new EvarSubstPointer EvarSubst @@ gen_constr EvarSubstValue h in
           ignore' @@ List.fold_left (fun prev b ->
               let* curr = move_toward_new EvarSubstPointer EvarSubst @@ gen_constr EvarSubstValue b in
               map (const curr) @@ mk_edge EvarSubstOrder prev curr)
             head substs)
    | Sort s ->
        (match s with
         | Sorts.SProp -> draw_toward_new' et SortSProp
         | Sorts.Prop -> draw_toward_new' et SortProp
         | Sorts.Set -> draw_toward_new' et SortSet
         | Sorts.Type _ -> draw_toward_new' et SortType)
    | Cast (term, kind, typ) ->
      move_toward_new' et Cast
        (gen_constr CastTerm term >>
         gen_constr CastType typ)
    | Prod (id, bi, conc) ->
      move_toward_new' et (Prod id.binder_name)
        (gen_constr ProdType bi >>
         let* prod = focus in
         with_relative prod @@ gen_constr ProdTerm conc)
    | Lambda (id, typ, term) ->
      move_toward_new' et (Lambda id.binder_name)
        (gen_constr LambdaType typ >>
         let* lambda = focus in
         with_relative lambda @@ gen_constr LambdaTerm term)
    | LetIn (id, def, typ, term) ->
      move_toward_new' et (LetIn id.binder_name)
        (gen_constr LetInDef def >>
         gen_constr LetInType typ >>
         let* letin = focus in
         with_relative letin @@ gen_constr LetInTerm term)
    | App (f, args) ->
      move_toward_new' et App @@
      let* fn = move_toward_new AppFunPointer AppFun @@ gen_constr AppFunValue f in
      ignore' @@ List.fold_left (fun prev b ->
          let* curr = move_toward_new AppArgPointer AppArg @@ gen_constr AppArgValue b in
          map (const curr) @@ mk_edge AppArgOrder prev curr)
        fn (Array.to_list args)
    | Const (c, u) ->
      follow_def et (mk_definition (ManualConst c)) @@ gen_const2 et c
    | Ind (i, u) ->
      follow_def et (mk_definition (Ind i)) @@ gen_inductive2 et i
    | Construct (c, u) ->
      follow_def et (mk_definition (Construct c)) @@ gen_constructor2 et c
    | Case (i, ret, term, branches) ->
      move_toward_new' et Case
        (follow_def CaseInd (mk_definition (Ind i.ci_ind)) @@ gen_inductive2 CaseInd i.ci_ind >>
         gen_constr CaseReturn ret >>
         gen_constr CaseTerm term >>
         List.iter (fun (c, branch) ->
             move_toward_new' CaseBranchPointer CaseBranch
               (follow_def CBConstruct (mk_definition (Construct (i.ci_ind, c + 1))) @@ gen_constructor2 CBConstruct (i.ci_ind, c + 1) >>
                gen_constr CBTerm branch))
           (OList.mapi (fun i x -> i, x) @@ Array.to_list branches))
    | Fix ((offset, ret), (ids, typs, terms)) ->
      move_toward_new' et Fix @@
      let* funs = List.map (fun id -> mk_node @@ FixFun id.binder_name) (Array.to_list ids) in
      let combined = OList.combine funs (OList.combine (Array.to_list typs) (Array.to_list terms)) in
      List.iter (fun (fn, (typ, term)) ->
          move_toward FixMutual fn
            (gen_constr FixFunType typ >>
             with_relatives funs @@ gen_constr FixFunTerm term))
        combined >>
      draw_toward FixReturn (OList.nth funs ret)
    | CoFix (ret, (ids, typs, terms)) ->
      move_toward_new' et CoFix @@
      let* funs = List.map (fun id -> mk_node @@ CoFixFun id.binder_name) (Array.to_list ids) in
      let combined = OList.combine funs (OList.combine (Array.to_list typs) (Array.to_list terms)) in
      List.iter (fun (fn, (typ, term)) ->
          move_toward CoFixMutual fn
            (gen_constr CoFixFunType typ >>
             with_relatives funs @@ gen_constr CoFixFunTerm term))
        combined >>
      draw_toward CoFixReturn (OList.nth funs ret)
    | Proj (p, term) ->
      move_toward_new' et App @@
      let* fn = move_toward_new AppFunPointer AppFun @@ follow_def AppFunValue (mk_definition (Proj (Projection.repr p))) @@ gen_projection AppFunValue p in
      let* arg = move_toward_new AppArgPointer AppArg @@ gen_constr AppArgValue term in
      mk_edge AppArgOrder fn arg
    | Int n ->
      draw_toward_new' et @@ Int n
    | Float f ->
      draw_toward_new' et @@ Float f

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
