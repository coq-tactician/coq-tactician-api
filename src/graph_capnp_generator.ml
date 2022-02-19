open Graph_def
open Graph_extractor
open Names
open Tactician_ltac1_record_plugin
open Ltac_plugin

let dirpath = Global.current_dirpath ()

(* TODO: See if this can be merged with SimpleGraph *)
type node = DirPath.t * int
type edge_label = edge_type
type node_label = node node_type
type children = (edge_label * node) list
module GlobalGraph : GraphMonadType
  with type node_label = node_label
   and type edge_label = edge_label
   and type node = node
   and type 'a repr_t = 'a * (node_label * children) list * DPset.t
= struct
  type node = DirPath.t * int
  type edge_label = edge_type
  type node_label = node node_type
  type children = (edge_label * node) list
  type writer =
    { nodes : (node_label * children) DList.t
    ; paths : DPset.t }
  module M = Monad_util.StateWriterMonad
      (struct type s = int end)
      (struct type w = writer
        let id = { nodes = DList.nil; paths = DPset.empty }
        let comb = fun
          { nodes = n1; paths = p1 }
          { nodes = n2; paths = p2 } ->
          { nodes = DList.append n1 n2; paths = DPset.union p1 p2 } end)
  include M
  type 'a repr_t = 'a * (node_label * children) list * DPset.t
  open Monad_util.WithMonadNotations(M)
  let index_to_node i = dirpath, i
  let children_paths ch ps =
    List.fold_left (fun ps (_, (p, _)) -> DPset.add p ps) ps ch
  let mk_node nl ch =
    let* i = get in
    put (i + 1) >>
    let+ () = tell { nodes = DList.singleton (nl, ch); paths = children_paths ch DPset.empty } in
    index_to_node i
  let with_delayed_node f =
    let* i = get in
    put (i + 1) >>
    pass @@
    let+ (v, nl, ch) = f (index_to_node i) in
    v, fun { nodes; paths } -> { nodes = DList.cons (nl, ch) nodes; paths = children_paths ch paths }
  let register_external (tp, _) =
    tell { nodes = DList.nil; paths = DPset.singleton tp }
  let run m =
    let _, ({ nodes; paths }, res) = run m 0 in
    res, DList.to_list nodes, paths
end


module G = GlobalGraph
module CICGraph = struct
  type node' = node
  include CICGraphMonad(GlobalGraph)
end
module GB = GraphBuilder(CICGraph)

module K = Graph_api.Make(Capnp.BytesMessage)
let nt2nt transformer (nt : G.node node_type) cnt =
  let open K.Builder.NodeClassification in
  match nt with
  | Root -> root_set cnt
  | ContextDef id -> context_def_set cnt (Id.to_string id)
  | ContextAssum id -> context_assum_set cnt (Id.to_string id)
  | Definition { previous; def_type } ->
    let cdef = definition_init cnt in
    let open K.Builder.Definition in
    (match def_type with
    | TacticalConstant (c, proof) ->
      let hash = Constant.UserOrd.hash c in
      hash_set cdef (Stdint.Uint64.of_int hash);
      name_set cdef (Constant.to_string c);
      let tconst = tactical_constant_init cdef in
      let arr = TacticalConstant.tactical_proof_init tconst (List.length proof) in
      List.iteri (fun i ({ tactic; base_tactic; interm_tactic; tactic_hash; arguments; root; context; ps_string }
                         : G.node tactical_step) ->
          let arri = Capnp.Array.get arr i in
          let state = K.Builder.ProofStep.state_init arri in
          let capnp_tactic = K.Builder.ProofStep.tactic_init arri in
          K.Builder.ProofState.root_set_int_exn state @@ snd root;
          let _ = K.Builder.ProofState.context_set_list state
              (List.map (fun x -> Stdint.Uint32.of_int @@ snd x) context) in
          K.Builder.ProofState.text_set state ps_string;
          K.Builder.Tactic.ident_set_int_exn capnp_tactic tactic_hash;
          K.Builder.Tactic.text_set capnp_tactic
            (Pp.string_of_ppcmds @@ Sexpr.format_oneline (Pptactic.pr_glob_tactic (Global.env ()) tactic));
          K.Builder.Tactic.base_text_set capnp_tactic
            (Pp.string_of_ppcmds @@ Sexpr.format_oneline (Pptactic.pr_glob_tactic (Global.env ()) base_tactic));
          K.Builder.Tactic.interm_text_set capnp_tactic
            (Pp.string_of_ppcmds @@ Sexpr.format_oneline (Pptactic.pr_glob_tactic (Global.env ()) interm_tactic));
          let arg_arr = K.Builder.Tactic.arguments_init capnp_tactic (List.length arguments) in
          List.iteri (fun i arg ->
              let arri = Capnp.Array.get arg_arr i in
              match arg with
              | None -> K.Builder.Tactic.Argument.unresolvable_set arri
              | Some (dep, index) ->
                let node = K.Builder.Tactic.Argument.term_init arri in
                K.Builder.Tactic.Argument.Term.dep_index_set_exn node @@ transformer dep;
                K.Builder.Tactic.Argument.Term.node_index_set_int_exn node index
            ) arguments;
          ()
        ) proof;
    | ManualConst c ->
      let hash = Constant.UserOrd.hash c in
      hash_set cdef (Stdint.Uint64.of_int hash);
      name_set cdef (Constant.to_string c);
      manual_constant_set cdef
    | Ind (m, i) ->
      let hash = Hashtbl.hash (i, MutInd.UserOrd.hash m) in
      hash_set cdef (Stdint.Uint64.of_int hash);
      name_set cdef (Libnames.string_of_path @@ Nametab.path_of_global (GlobRef.IndRef (m, i)));
      inductive_set cdef
    | Construct ((m, i), j) ->
      let hash = Hashtbl.hash (i, j, MutInd.UserOrd.hash m) in
      hash_set cdef (Stdint.Uint64.of_int @@ hash);
      name_set cdef (Libnames.string_of_path @@ Nametab.path_of_global (GlobRef.ConstructRef ((m, i), j)));
      constructor_set cdef
    | Proj p ->
      let hash = Projection.Repr.UserOrd.hash p in
      hash_set cdef @@ Stdint.Uint64.of_int hash; (* TODO: This is probably not a good hash *)
      name_set cdef (projection_to_string p); (* TODO: Better name *)
      projection_set cdef
    )
  | ConstEmpty -> const_empty_set cnt
  | SortSProp -> sort_s_prop_set cnt
  | SortProp -> sort_prop_set cnt
  | SortSet -> sort_set_set cnt
  | SortType -> sort_type_set cnt
  | Rel -> rel_set cnt
  | Var -> var_set cnt
  | Evar i -> evar_set cnt @@ Stdint.Uint64.of_int i
  | EvarSubst -> evar_subst_set cnt
  | Cast -> cast_set cnt
  | Prod _ -> prod_set cnt
  | Lambda _ -> lambda_set cnt
  | LetIn _ -> let_in_set cnt
  | App -> app_set cnt
  | AppFun -> app_fun_set cnt
  | AppArg -> app_arg_set cnt
  | Case -> case_set cnt
  | CaseBranch -> case_branch_set cnt
  | Fix -> fix_set cnt
  | FixFun _ -> fix_fun_set cnt
  | CoFix -> co_fix_set cnt
  | CoFixFun _ -> co_fix_fun_set cnt
  | Int i -> int_set cnt @@ Stdint.Uint64.of_int @@ snd @@ Uint63.to_int2 i
  | Float f -> float_set cnt (float64_to_float f)
  | Primitive p -> primitive_set cnt (CPrimitives.to_string p)
let et2et (et : edge_type) =
  let open K.Builder.EdgeClassification in
  match et with
  | ContextElem -> ContextElem
  | ContextSubject -> ContextSubject
  | ContextDefType -> ContextDefType
  | ContextDefTerm -> ContextDefTerm
  | ConstType -> ConstType
  | ConstUndef -> ConstUndef
  | ConstDef -> ConstDef
  | ConstOpaqueDef -> ConstOpaqueDef
  | ConstPrimitive -> ConstPrimitive
  | IndType -> IndType
  | IndConstruct -> IndConstruct
  | ProjTerm -> ProjTerm
  | ConstructTerm -> ConstructTerm
  | CastTerm -> CastTerm
  | CastType -> CastType
  | ProdType -> ProdType
  | ProdTerm -> ProdTerm
  | LambdaType -> LambdaType
  | LambdaTerm -> LambdaTerm
  | LetInDef -> LetInDef
  | LetInType -> LetInType
  | LetInTerm -> LetInTerm
  | AppFunPointer -> AppFunPointer
  | AppFunValue -> AppFunValue
  | AppArgPointer -> AppArgPointer
  | AppArgValue -> AppArgValue
  | AppArgOrder -> AppArgOrder
  | CaseTerm -> CaseTerm
  | CaseReturn -> CaseReturn
  | CaseBranchPointer -> CaseBranchPointer
  | CaseInd -> CaseInd
  | CBConstruct -> CBConstruct
  | CBTerm -> CBTerm
  | FixMutual -> FixMutual
  | FixReturn -> FixReturn
  | FixFunType -> FixFunType
  | FixFunTerm -> FixFunTerm
  | CoFixMutual -> CoFixMutual
  | CoFixReturn -> CoFixReturn
  | CoFixFunType -> CoFixFunType
  | CoFixFunTerm -> CoFixFunTerm
  | RelPointer -> RelPointer
  | VarPointer -> VarPointer
  | EvarSubstPointer -> EvarSubstPointer
  | EvarSubstOrder -> EvarSubstOrder
  | EvarSubstValue -> EvarSubstValue

let write_graph capnp_graph transformer nodes =
  let cnodes = K.Builder.Graph.heap_init capnp_graph (List.length nodes) in
  List.iteri (fun i (label, children) ->
      let cnode = Capnp.Array.get cnodes i in
      nt2nt transformer label @@ K.Builder.Graph.Node.label_get cnode;
      let cchildren = K.Builder.Graph.Node.children_init cnode (List.length children) in
      List.iteri (fun i (label, (tp, ti)) ->
          let et = Capnp.Array.get cchildren i in
          K.Builder.Graph.EdgeTarget.label_set et @@ et2et label;
          let ctarget = K.Builder.Graph.EdgeTarget.target_get et in
          K.Builder.Graph.EdgeTarget.Target.dep_index_set_exn ctarget @@ transformer tp;
          K.Builder.Graph.EdgeTarget.Target.node_index_set_int_exn ctarget @@ ti
        ) children
    ) nodes
