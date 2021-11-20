open Labelled_graph_def
open Labelled_graph_extractor
open Names
open Tactician_ltac1_record_plugin
open Ltac_plugin

let dirpath = Global.current_dirpath ()
module GlobalGraph = struct
  type node = DirPath.t * int
  type edge_label = edge_type
  type node_label = node node_type
  type 'loc directed_edge =
    { from : int
    ; sort : edge_type
    ; toward : 'loc * int }
  type t =
    { nodes : node node_type list
    ; paths : DPset.t
    ; last  : int
    ; assoc : DirPath.t directed_edge list }
  let empty = { nodes = []; paths = DPset.empty; last = 0; assoc = [] }
  let index_to_node i = dirpath, i
  let mk_node ({ nodes; last; _ } as g) typ =
    index_to_node last, { g with nodes = typ::nodes; last = last + 1}
  let mk_edge ({ assoc; paths; _ } as g) sort ~source:(fp, fi) ~target:(tp, ti) =
    assert (fp = dirpath);
    { g with
      assoc = { from = fi; sort; toward = (tp, ti) } :: assoc
    ; paths = DPset.add tp paths }
  let node_list { nodes; _ } = List.rev nodes
  let edge_list { assoc; _ } = List.rev assoc
end
module G = GlobalGraph
module GB = GraphBuilder(G)

module K = Labelled_graph_api.Make(Capnp.BytesMessage)
let nt2nt transformer (nt : G.node node_type) cnt =
  let open K.Builder.NodeClassification in
  match nt with
  | Root -> root_set cnt
  | ContextDef _ -> context_def_set cnt
  | ContextAssum _ -> context_assum_set cnt
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
      List.iteri (fun i ({ tactic; base_tactic; tactic_hash; arguments; root; context; ps_string }
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
          let arg_arr = K.Builder.Tactic.arguments_init capnp_tactic (List.length arguments) in
          List.iteri (fun i (dep, index) ->
              let arri = Capnp.Array.get arg_arr i in
              K.Builder.GlobalNode.dep_index_set_exn arri @@ transformer dep;
              K.Builder.GlobalNode.node_index_set_int_exn arri index
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

let write_classification_list transformer classification_arr nodes =
  let arr = classification_arr (List.length nodes) in
  List.iteri (fun i x ->
      let arri = Capnp.Array.get arr i in
      nt2nt transformer x arri) nodes

let write_edge_list transformer edge_arr edges =
  let open G in
  let arr = edge_arr (List.length @@ edges) in
  List.iteri (fun i { from; sort; toward=(tp, ti) } ->
      let arri = Capnp.Array.get arr i in
      K.Builder.DirectedEdge.source_set_int_exn arri from;
      K.Builder.DirectedEdge.sort_set arri @@ et2et sort;
      let toward = K.Builder.DirectedEdge.target_get arri in
      K.Builder.DirectedEdge.Target.dep_index_set_exn toward @@ transformer tp;
      K.Builder.DirectedEdge.Target.node_index_set_int_exn toward ti
    ) edges

let write_graph capnp_graph transformer nodes edges =
  let arr = K.Builder.Graph.classifications_init capnp_graph in
  write_classification_list transformer arr nodes;
  let arr = K.Builder.Graph.edges_init capnp_graph in
  write_edge_list transformer arr edges
