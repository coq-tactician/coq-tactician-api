open Labelled_graph_def
open Labelled_graph_extractor
open Names

let dirpath = Global.current_dirpath ()
module GlobalGraph = struct
  type node = DirPath.t * int
  type 'loc directed_edge =
    { from : int
    ; sort : edge_type
    ; toward : 'loc * int }
  type t =
    { nodes : node_type list
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
let nt2nt (nt : node_type) cnt =
  let open K.Builder.NodeClassification in
  match nt with
  | Root -> root_set cnt
  | ContextDef _ -> context_def_set cnt
  | ContextAssum _ -> context_assum_set cnt
  | Const c -> const_set cnt (Stdint.Uint64.of_int @@ Constant.hash c)
  | ConstEmpty -> const_empty_set cnt
  | Ind (m, i) -> ind_set cnt (Stdint.Uint64.of_int @@ (Hashtbl.hash (i, MutInd.hash m)))
  | Construct ((m, i), j) -> construct_set cnt (Stdint.Uint64.of_int @@ (Hashtbl.hash (i, j, MutInd.hash m)))
  | Proj p -> proj_set cnt @@ Stdint.Uint64.of_int @@ Projection.Repr.hash p (* TODO: This is probably not a good hash *)
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

let write_classification_list classification_arr nodes =
  let arr = classification_arr (List.length nodes) in
  List.iteri (fun i x ->
      let arri = Capnp.Array.get arr i in
      nt2nt x arri) nodes

let write_edge_list edge_arr edges =
  let open G in
  let arr = edge_arr (List.length @@ edges) in
  List.iteri (fun i { from; sort; toward=(tp, ti) } ->
      let arri = Capnp.Array.get arr i in
      K.Builder.DirectedEdge.source_set_int_exn arri from;
      K.Builder.DirectedEdge.sort_set arri @@ et2et sort;
      let toward = K.Builder.DirectedEdge.target_get arri in
      K.Builder.DirectedEdge.Target.dep_index_set_exn toward @@ tp;
      K.Builder.DirectedEdge.Target.node_index_set_int_exn toward ti
    ) edges

let write_graph capnp_graph nodes edges =
  let arr = K.Builder.Graph.classifications_init capnp_graph in
  write_classification_list arr nodes;
  let arr = K.Builder.Graph.edges_init capnp_graph in
  write_edge_list arr edges
