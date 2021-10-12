open Tactician_ltac1_record_plugin
open Graph_def
open Names
open Declarations
open Context

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
    ; follow_defs : bool }

  module M = ReaderStateMonad(struct type s = state and r = context end)
  open M
  open Tactician_util.WithMonadNotations(M)
  module OList = List
  open Monad.Make(M)

  let run_empty follow_defs m =
    let focus, graph = G.mk_node G.empty Root in
    let state =
      { graph
      ; constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = ProjMap.empty} in
    let context = { relative = []; named = Id.Map.empty; focus; follow_defs } in
    m (state, context)

  let const x _ = x
  let ignore' m = map ignore m

  let mk_node nt =
    let* ({ graph; _ } as g) = get in
    let n, graph = G.mk_node graph nt in
    let+ () = put { g with graph } in
    n
  let mk_edge from toward =
    let* { graph; _ } as g = get in
    put { g with graph = G.mk_edge graph ~from ~toward }
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
  let with_relatives ns =
    local (fun ({ relative; _ } as c) ->
        let relative = OList.fold_left (fun ctx n -> n::ctx) ns relative in (* Funs are added backwards *)
        { c with relative })
  let with_reset m =
    local (fun c -> { c with named = Id.Map.empty; relative = [] }) m

  let draw_toward n =
    let* { focus; _ } = ask in
    mk_edge focus n
  let move_toward n m =
    draw_toward n >>
    local (fun c -> { c with focus = n }) m
  let move_toward_new nt m =
    let* n = mk_node nt in
    move_toward n m >> return n
  let move_toward_new' nt m = ignore' @@ move_toward_new nt m
  let draw_toward_new nt =
    move_toward_new nt (return ())
  let draw_toward_new' nt = move_toward_new' nt (return ())

  let register_ind i n =
    let* ({ inductives; _ } as s) = get in
    put { s with inductives = Indmap.add i n inductives }
  let register_construct c n =
    let* ({ constructors; _ } as s) = get in
    put { s with constructors = Constrmap.add c n constructors }
  let register_projection p n =
    let* ({ projections; _ } as s) = get in
    put { s with projections = ProjMap.add p n projections }

  let cached_gen_const c gen =
    let* { constants; _ } = get in
    match Cmap.find_opt c constants with
    | Some c -> draw_toward c
    | None ->
      let* n = with_reset gen in
      let* ({ constants; _ } as s) = get in
      put { s with constants = Cmap.add c n constants }

  let rec with_named_context c (m : unit t) =
    Named.fold_inside (fun m d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          let* var = move_toward_new (LocalAssum id.binder_name) @@ gen_constr typ in
          with_var id.binder_name var m
        | Named.Declaration.LocalDef (id, term, typ) ->
          let* var = move_toward_new (LocalDef id.binder_name)
            (move_toward_new' LocalDefType @@ gen_constr typ >>
             move_toward_new' LocalDefTerm @@ gen_constr term) in
          with_var id.binder_name var m)
      c ~init:m
  and with_named_context' c (m : 'a t) =
    Named.fold_inside (fun m d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          let* var = move_toward_new (LocalAssum id.binder_name) @@ gen_constr typ in
          with_var id.binder_name var m
        | Named.Declaration.LocalDef (id, term, typ) ->
          let* var = move_toward_new (LocalDef id.binder_name)
            (move_toward_new' LocalDefType @@ gen_constr typ >>
             move_toward_new' LocalDefTerm @@ gen_constr term) in
          with_var id.binder_name var m)
      c ~init:m
  and gen_const c =
      cached_gen_const c @@ move_toward_new (Const c) @@
      let { const_hyps; const_body; const_type; _ } = Global.lookup_constant c in
      with_named_context const_hyps
        (move_toward_new' ConstType @@ gen_constr const_type >>
         match const_body with
         | Undef _ -> draw_toward_new' ConstUndef
         | Def c -> move_toward_new' ConstDef @@ gen_constr @@ Mod_subst.force_constr c
         | OpaqueDef c ->
           let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Global.opaque_tables ()) c in
           move_toward_new' ConstOpaqueDef @@ gen_constr c
         | Primitive p -> draw_toward_new' @@ Primitive p)
  and gen_primitive_constructor ind proj_npars typ =
    let relctx, sort = Term.decompose_prod_assum typ in
    let real_arity = Rel.nhyps @@ CList.skipn proj_npars @@ OList.rev relctx in
    snd @@ CList.fold_left (fun (reli, m) d ->
        match d with
        | Rel.Declaration.LocalAssum ({ binder_name = id; _ }, typ) ->
          reli - 1, move_toward_new' (Prod id)
            (let* prod = focus in
             (match id with
              | Name id when reli >= 0 ->
                let proj = Projection.Repr.make
                    ind ~proj_npars ~proj_arg:reli @@ Label.of_id id in
                let* nproj = mk_node (Proj proj) in
                register_projection proj nproj >>
                with_focus nproj @@ draw_toward prod
              | _ -> return ()) >>
             move_toward_new' ProdType @@ gen_constr typ >>
             with_relative prod @@ move_toward_new' ProdTerm m)
        | Rel.Declaration.LocalDef ({ binder_name = id; _ }, term, typ) ->
          reli, move_toward_new' (LetIn id)
            (move_toward_new' LetInType @@ gen_constr typ >>
             move_toward_new' LetInDef @@ gen_constr term >>
             let* letin = focus in
             with_relative letin @@ move_toward_new' LetInDef m))
      (real_arity - 1, gen_constr sort) relctx
  and gen_mutinductive_helper m =
    let* { inductives; _ } = get in
    match Indmap.find_opt (m, 0) inductives with
    | Some mn -> return ()
    | None ->
      with_reset @@
      let ({ mind_hyps; mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
        Global.lookup_mind m in
      with_named_context mind_hyps @@
      let inds = OList.mapi (fun i ind -> i, ind) (Array.to_list mind_packets) in
      let* inds = List.map (fun (i, ind) ->
          let* n = mk_node @@ Ind (m, i) in
          register_ind (m, i) n >> return (i, ind, n)) inds in
      let indsn = OList.rev @@ OList.map (fun (_, _, n) -> n) inds in (* Backwards ordering w.r.t. Fun *)
      List.iter (fun (i, ({ mind_user_lc; mind_consnames; _ } as ib), n) ->
          let gen_constr_typ typ = match mind_record with
            | NotRecord | FakeRecord -> gen_constr typ
            | PrimRecord _ -> gen_primitive_constructor (m, i) (OList.length mind_params_ctxt) typ in
          let constructs = OList.mapi (fun j x -> j, x) @@
            OList.combine (Array.to_list mind_user_lc) (Array.to_list mind_consnames) in
          with_focus n
            (List.iter (fun (j, (typ, id)) ->
                 with_relatives indsn @@ move_toward_new (Construct ((m, i), j + 1)) @@ gen_constr_typ typ >>=
                 register_construct ((m, i), j + 1)) constructs >>
             let univs = Declareops.inductive_polymorphic_context mb in
             let inst = Univ.make_abstract_instance univs in
             let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) (Global.env ()) in
             let typ = Inductive.type_of_inductive env ((mb, ib), inst) in
             gen_constr typ)) inds
and gen_inductive ((m, _) as i) =
    gen_mutinductive_helper m >>
    let* { inductives; _ } = get in
    match Indmap.find_opt i inductives with
    | Some inn -> draw_toward inn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and gen_constructor (((m, _), _) as c) =
    gen_mutinductive_helper m >>
    let* { constructors; _ } = get in
    match Constrmap.find_opt c constructors with
    | Some cn -> draw_toward cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and gen_projection p =
    gen_mutinductive_helper (Projection.mind p) >>
    let* { projections; _ } = get in
    match ProjMap.find_opt (Projection.repr p) projections with
    | Some cn -> draw_toward cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem")
  and follow_def nt def =
    let* ({ follow_defs; _ }) = ask in
    if follow_defs then def else draw_toward_new' nt
  and gen_constr  c = gen_kind_of_term @@ Constr.kind c
  and gen_kind_of_term = function
    | Rel i ->
      let* ino = relative_lookup i in
      move_toward_new' Rel @@ draw_toward ino
    | Var id ->
      move_toward_new' Var (named_lookup id >>= draw_toward)
    | Meta i ->
      CErrors.anomaly (Pp.str "Unexpected meta")
    | Evar (ev, substs) ->
      move_toward_new' (Evar (Evar.repr ev))
        (match Array.to_list substs with
         | [] -> return ()
         | h::substs ->
           let* head = move_toward_new EvarSubst @@ gen_constr h in
           ignore' @@ List.fold_left (fun prev b ->
               let* curr = move_toward_new EvarSubst @@ gen_constr b in
               map (const curr) @@ mk_edge prev curr)
             head substs)
    | Sort s ->
      move_toward_new' Sort
        (match s with
         | Sorts.SProp -> draw_toward_new' SProp
         | Sorts.Prop -> draw_toward_new' Prop
         | Sorts.Set -> draw_toward_new' Set
         | Sorts.Type _ -> draw_toward_new' Type)
    | Cast (term, kind, typ) ->
      move_toward_new' Cast
        (move_toward_new' CastTerm @@ gen_constr term >>
         move_toward_new' CastType @@ gen_constr typ)
    | Prod (id, bi, conc) ->
      move_toward_new' (Prod id.binder_name)
        (move_toward_new' ProdType @@ gen_constr bi >>
         let* prod = focus in
         move_toward_new' ProdTerm @@ with_relative prod @@ gen_constr conc)
    | Lambda (id, typ, term) ->
      move_toward_new' (Lambda id.binder_name)
        (move_toward_new' LambdaType @@ gen_constr typ >>
         let* lambda = focus in
         move_toward_new' LambdaTerm @@ with_relative lambda @@ gen_constr term)
    | LetIn (id, def, typ, term) ->
      move_toward_new' (LetIn id.binder_name)
        (move_toward_new' LetInDef @@ gen_constr def >>
         move_toward_new' LetInType @@ gen_constr typ >>
         let* letin = focus in
         move_toward_new' LetInTerm @@ with_relative letin @@ gen_constr term)
    | App (f, args) ->
      move_toward_new' App @@
      let* fn = move_toward_new AppFun @@ gen_constr f in
      ignore' @@ List.fold_left (fun prev b ->
          let* curr = move_toward_new AppArg @@ gen_constr b in
          map (const curr) @@ mk_edge prev curr)
        fn (Array.to_list args)
    | Const (c, u) ->
      follow_def (Const c) @@ gen_const c
    | Ind (i, u) ->
      follow_def (Ind i) @@ gen_inductive i
    | Construct (c, u) ->
      follow_def (Construct c) @@ gen_constructor c
    | Case (i, ret, term, branches) ->
      move_toward_new' Case
        (follow_def (Ind i.ci_ind) @@ gen_inductive i.ci_ind >>
         move_toward_new' CaseReturn @@ gen_constr ret >>
         move_toward_new' CaseTerm @@ gen_constr term >>
         List.iter (fun (c, branch) ->
             move_toward_new' CaseBranch
               (move_toward_new' CBConstruct @@
                follow_def (Construct (i.ci_ind, c + 1)) @@ gen_constructor (i.ci_ind, c + 1) >>
                move_toward_new' CBTerm @@ gen_constr branch))
           (OList.mapi (fun i x -> i, x) @@ Array.to_list branches))
    | Fix ((offset, ret), (ids, typs, terms)) ->
      move_toward_new' Fix @@
      let* funs = List.map (fun id -> mk_node @@ FixFun id.binder_name) (Array.to_list ids) in
      let combined = OList.combine funs (OList.combine (Array.to_list typs) (Array.to_list terms)) in
      List.iter (fun (fn, (typ, term)) ->
          move_toward fn
            (move_toward_new' FixFunType @@ gen_constr typ >>
             move_toward_new' FixFunTerm @@ with_relatives funs @@ gen_constr term))
        combined >>
      move_toward_new' FixReturn @@ draw_toward (OList.nth funs ret)
    | CoFix (ret, (ids, typs, terms)) ->
      move_toward_new' CoFix @@
      let* funs = List.map (fun id -> mk_node @@ CoFixFun id.binder_name) (Array.to_list ids) in
      let combined = OList.combine funs (OList.combine (Array.to_list typs) (Array.to_list terms)) in
      List.iter (fun (fn, (typ, term)) ->
          move_toward fn
            (move_toward_new' CoFixFunType @@ gen_constr typ >>
             move_toward_new' CoFixFunTerm @@ with_relatives funs @@ gen_constr term))
        combined >>
      move_toward_new' CoFixReturn @@ draw_toward (OList.nth funs ret)
    | Proj (p, term) ->
      move_toward_new' App @@
      let* fn = move_toward_new AppFun @@ follow_def (Proj (Projection.repr p)) @@ gen_projection p in
      let* arg = move_toward_new AppArg @@ gen_constr term in
      mk_edge fn arg
    | Int n ->
      draw_toward_new' @@ Int n
    | Float f ->
      draw_toward_new' @@ Float f

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
        let ret = with_named_context hyps @@ gen_constr (EConstr.to_constr sigma concl)
      in Proofview.tclUNIT ret)

  let gen_globref = function
    | GlobRef.VarRef _ -> CErrors.anomaly (Pp.str "not handled yet")
    | GlobRef.ConstRef c -> gen_const c
    | GlobRef.IndRef i -> gen_inductive i
    | GlobRef.ConstructRef c -> gen_constructor c
end
