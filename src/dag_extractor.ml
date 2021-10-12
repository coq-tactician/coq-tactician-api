open Tactician_ltac1_record_plugin
open Dag_def
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

module DAGBuilder(G : DAG) = struct

  module ProjMap = CMap.Make(Projection.Repr)

  type state =
    { dag         : G.t
    ; constants     : G.node Cmap.t
    ; inductives    : G.node Indmap.t
    ; constructors  : G.node Constrmap.t
    ; projections   : G.node ProjMap.t }

  type context =
    { relative    : G.node list
    ; named       : G.node Id.Map.t
    ; follow_defs : bool }


  let remove_nth i l =
    CList.nth l i, (CList.firstn i l @ CList.skipn (i+1) l)

  module M = ReaderStateMonad(struct type s = state and r = context end)
  open M
  open Tactician_util.WithMonadNotations(M)
  module OList = List
  open Monad.Make(M)

  let run_empty follow_defs m =
    let dag = G.empty in
    let state =
      { dag
      ; constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = ProjMap.empty} in
    let context = { relative = []; named = Id.Map.empty; follow_defs } in
    m (state, context)

  let const x _ = x
  let ignore' m = map ignore m

  let mk_node nt c =
    let* ({ dag; _ } as g) = get in
    let n, dag = G.mk_node dag nt c in
    let+ () = put { g with dag } in
    n
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
  let with_relatives ns =
    local (fun ({ relative; _ } as c) ->
        let relative = OList.fold_left (fun ctx n -> n::ctx) ns relative in (* Funs are added backwards *)
        { c with relative })
  let with_reset m =
    local (fun c -> { c with named = Id.Map.empty; relative = [] }) m

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
    | Some c -> return c
    | None ->
      let* n = with_reset gen in
      let* ({ constants; _ } as s) = get in
      put { s with constants = Cmap.add c n constants } >> return n

  let rec with_named_context c (m : unit t) : unit t =
    OList.fold_left (fun m d ->
        match d with
        | Named.Declaration.LocalAssum (id, typ) ->
          let* typ = gen_constr typ in
          let* var = mk_node (ContextAssum id.binder_name) [typ] in
          with_var id.binder_name var m
        | Named.Declaration.LocalDef (id, term, typ) ->
          let* typ = gen_constr typ in
          let* term = gen_constr term in
          let* var = mk_node (ContextDef id.binder_name) [typ; term] in
          with_var id.binder_name var m)
      m c
  and with_named_context2 ?force:(force=false) c (m : G.node t) : G.node t =
    if CList.is_empty c && not force then m else
      let* ctx, subject = OList.fold_left (fun m d ->
          match d with
          | Named.Declaration.LocalAssum (id, typ) ->
            let* typ = gen_constr typ in
            let* var = mk_node (ContextAssum id.binder_name) [typ] in
            let+ ctx, subj = with_var id.binder_name var m in
            var::ctx, subj
          | Named.Declaration.LocalDef (id, term, typ) ->
            let* typ = gen_constr typ in
            let* term = gen_constr term in
            let* var = mk_node (ContextDef id.binder_name) [typ; term] in
            let+ ctx, subj = with_var id.binder_name var m in
            var::ctx, subj)
          (m >>= fun m -> return ([], m)) c in
      let* ctx = mk_node Context ctx in
      mk_node WithContext [ctx; subject]
  and gen_const c =
    cached_gen_const c @@
    let { const_hyps; const_body; const_type; _ } = Global.lookup_constant c in
    with_named_context2 const_hyps @@
    let* typ = gen_constr const_type in
    match const_body with
    | Undef _ -> mk_node (ConstUndef c) [typ]
    | Def c' ->
      let* term = gen_constr @@ Mod_subst.force_constr c' in
      mk_node (ConstDef c) [typ; term]
    | OpaqueDef c' ->
      let c', _ = Opaqueproof.force_proof Library.indirect_accessor (Global.opaque_tables ()) c' in
      let* term = gen_constr c' in
      mk_node (ConstOpaqueDef c) [typ; term]
    | Primitive p -> mk_node (ConstPrimitive p) []
  and gen_primitive_constructor ind proj_npars typ =
    let relctx, sort = Term.decompose_prod_assum typ in
    let real_arity = Rel.nhyps @@ CList.skipn proj_npars @@ OList.rev relctx in
    snd @@ CList.fold_left (fun (reli, m) d ->
        match d with
        | Rel.Declaration.LocalAssum ({ binder_name = id; _ }, typ) ->
          reli - 1,
          let* typ = gen_constr typ in
          let* term = with_relative typ @@ m in
          let* prod = mk_node (Prod id) [typ; term] in
          (match id with
           | Name id when reli >= 0 ->
             let proj = Projection.Repr.make
                 ind ~proj_npars ~proj_arg:reli @@ Label.of_id id in
             let* nproj = mk_node (Proj proj) [prod] in
             register_projection proj nproj
           | _ -> return ()) >>
          return prod
        | Rel.Declaration.LocalDef ({ binder_name = id; _ }, def, typ) ->
          reli,
          let* typ = gen_constr typ in
          let* def = gen_constr def in
          let* term = with_relative def @@ m in
          mk_node (LetIn id) [def; typ; term])
      (real_arity - 1, gen_constr sort) relctx
  and gen_mutinductive_helper m =
    let* { inductives; _ } = get in
    match Indmap.find_opt (m, 0) inductives with
    | Some mn -> return ()
    | None ->
      with_reset @@
      let ({ mind_hyps; mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
        Global.lookup_mind m in
      with_named_context mind_hyps @@ (* TODO: Should this use with_named_context2 *)
      let inds = OList.mapi (fun i ind -> i, ind) (Array.to_list mind_packets) in
      let* indsn = List.map (fun (i, ind) ->
          let univs = Declareops.inductive_polymorphic_context mb in
          let inst = Univ.make_abstract_instance univs in
          let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) (Global.env ()) in
          let typ = Inductive.type_of_inductive env ((mb, ind), inst) in
          let* typ = gen_constr typ in
          mk_node IndBinder [typ]) inds in
      let inds = OList.combine inds indsn in
      let indsn = OList.rev indsn in (* Backwards ordering w.r.t. Fun *)
      List.iter (fun ((i, { mind_user_lc; mind_consnames; _ }), typ) ->
          let gen_constr_typ typ = match mind_record with
            | NotRecord | FakeRecord -> gen_constr typ
            | PrimRecord _ -> gen_primitive_constructor (m, i) (OList.length mind_params_ctxt) typ in
          let constructs = OList.mapi (fun j x -> j, x) @@
            OList.combine (Array.to_list mind_user_lc) (Array.to_list mind_consnames) in
          let* constructs = List.map (fun (j, (typ, id)) ->
               let* constr_typ = with_relatives indsn @@ gen_constr_typ typ in
               let* constr = mk_node (Construct ((m, i), j + 1)) [constr_typ] in
               register_construct ((m, i), j + 1) constr >> return constr) constructs in
          let* constructs = mk_node IndConstructs constructs in
          let* ind = mk_node (Ind (m, i)) [typ; constructs] in
          register_ind (m, i) ind
          ) inds
and gen_inductive ((m, _) as i) =
    gen_mutinductive_helper m >>
    let* { inductives; _ } = get in
    match Indmap.find_opt i inductives with
    | Some inn -> return inn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem1")
  and gen_constructor (((m, _), _) as c) =
    gen_mutinductive_helper m >>
    let* { constructors; _ } = get in
    match Constrmap.find_opt c constructors with
    | Some cn -> return  cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem2")
  and gen_projection p =
    gen_mutinductive_helper (Projection.mind p) >>
    let* { projections; _ } = get in
    match ProjMap.find_opt (Projection.repr p) projections with
    | Some cn -> return cn
    | None -> CErrors.anomaly (Pp.str "Inductive generation problem3")
  and follow_def nt def =
    let* ({ follow_defs; _ }) = ask in
    if follow_defs then def else mk_node nt []
  and gen_constr : Constr.t -> G.node t = fun c -> gen_kind_of_term @@ Constr.kind c
  and gen_kind_of_term : (Constr.t, Constr.t, Sorts.t, Univ.Instance.t) Constr.kind_of_term -> G.node t = function
    | Constr.Rel i ->
      relative_lookup i
    | Var id ->
      named_lookup id
    | Meta i ->
      CErrors.anomaly (Pp.str "Unexpected meta")
    | Evar (ev, substs) ->
      let* substs = List.map (fun t -> gen_constr t) (Array.to_list substs) in
      let* substs = mk_node EvarSubsts substs in
      let* ev = mk_node (EvarId (Evar.repr ev)) [] in
      mk_node Evar [substs; ev]
    | Sort s ->
        mk_node (match s with
         | Sorts.SProp -> SortSProp
         | Sorts.Prop -> SortProp
         | Sorts.Set -> SortSet
         | Sorts.Type _ -> SortType) []
    | Cast (term, kind, typ) ->
      let* term = gen_constr term
      and+ typ = gen_constr typ in
      mk_node Cast [term; typ]
    | Prod (id, bi, conc) ->
      let* bi = gen_constr bi in
      let* binder = mk_node ProdBinder [bi] in
      let* conc = with_relative binder @@ gen_constr conc in
      mk_node (Prod id.binder_name) [binder; conc]
    | Lambda (id, typ, term) ->
      let* typ = gen_constr typ in
      let* binder = mk_node LambdaBinder [typ] in
      let* term = with_relative binder @@ gen_constr term in
      mk_node (Lambda id.binder_name) [binder; term]
    | LetIn (id, def, typ, term) ->
      let* def = gen_constr def in
      let* typ = gen_constr typ in
      let* binder = mk_node LetInBinder [typ; def] in
      let* term = with_relative binder @@ gen_constr term in
      mk_node (LetIn id.binder_name) [binder; term]
    | App (f, args) ->
      let* f = gen_constr f in
      let* args = List.map gen_constr (Array.to_list args) in
      let* args = mk_node AppArgs args in
      mk_node App [f; args]
    | Const (c, u) ->
      follow_def (ConstDef c) @@ gen_const c
    | Ind (i, u) ->
      follow_def (Ind i) @@ gen_inductive i
    | Construct (c, u) ->
      follow_def (Construct c) @@ gen_constructor c
    | Case (i, ret, term, branches) ->
      let* ret = gen_constr ret in
      let* term = gen_constr term in
      let* branches = List.map (fun (c, branch) ->
          let* branch = gen_constr branch in
          let* c = follow_def (Construct (i.ci_ind, c + 1)) @@ gen_constructor (i.ci_ind, c + 1) in
          mk_node CaseBranch [c; branch])
          (OList.mapi (fun i x -> i, x) @@ Array.to_list branches) in
      let* branches = mk_node CaseBranches branches in
      mk_node Case [ret; term; branches]
    | Fix ((offset, ret), (ids, typs, terms)) ->
      let* typs = List.map (fun typ ->
          let* typ = gen_constr typ in
          mk_node FixFunBinder [typ]) (Array.to_list typs) in
      let combined = OList.combine (Array.to_list ids)
          (OList.combine typs (Array.to_list terms)) in
      let (mainid, (maintyp, mainf)), auxes = remove_nth ret combined in
      let* auxes = List.map (fun (id, (typ, f)) ->
          let* f = with_relatives typs @@ gen_constr f in
          mk_node (FixFun id.binder_name) [typ; f]) auxes in
      let* auxes = mk_node FixAuxes auxes in
      let* mainf = with_relatives typs @@ gen_constr mainf in
      let* mainf = mk_node (FixFun mainid.binder_name) [maintyp; mainf] in
      mk_node Fix [mainf; auxes]
    | CoFix (ret, (ids, typs, terms)) ->
      let* typs = List.map (fun typ ->
          let* typ = gen_constr typ in
          mk_node CoFixFunBinder [typ]) (Array.to_list typs) in
      let combined = OList.combine (Array.to_list ids)
          (OList.combine typs (Array.to_list terms)) in
      let (mainid, (maintyp, mainf)), auxes = remove_nth ret combined in
      let* auxes = List.map (fun (id, (typ, f)) ->
          let* f = with_relatives typs @@ gen_constr f in
          mk_node (CoFixFun id.binder_name) [typ; f]) auxes in
      let* auxes = mk_node CoFixAuxes auxes in
      let* mainf = with_relatives typs @@ gen_constr mainf in
      let* mainf = mk_node (CoFixFun mainid.binder_name) [maintyp; mainf] in
      mk_node CoFix [mainf; auxes]
    | Proj (p, term) ->
      let* fn = follow_def (Proj (Projection.repr p)) @@ gen_projection p in
      let* arg = gen_constr term in
      mk_node App [fn; arg]
    | Int n ->
      mk_node (Int n) []
    | Float f ->
      mk_node (Float f) []

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
        let ret = with_named_context2 ~force:true hyps @@ gen_constr (EConstr.to_constr sigma concl)
      in Proofview.tclUNIT ret)

  let gen_globref = function
    | GlobRef.VarRef _ -> CErrors.anomaly (Pp.str "not handled yet")
    | GlobRef.ConstRef c -> gen_const c
    | GlobRef.IndRef i -> gen_inductive i
    | GlobRef.ConstructRef c -> gen_constructor c
end
