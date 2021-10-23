open Graph_def
open Graph_extractor
open Names

let dirpath = Global.current_dirpath ()
module GlobalGraph = struct
  type node = DirPath.t * int
  type 'loc directed_edge =
    { from : int
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
  let mk_edge ({ assoc; paths; _ } as g) ~from:(fp, fi) ~toward:(tp, ti) =
    assert (fp = dirpath);
    { g with
      assoc = { from = fi; toward = (tp, ti) } :: assoc
    ; paths = DPset.add tp paths }
  let node_list { nodes; _ } = List.rev nodes
  let edge_list { assoc; _ } = List.rev assoc
end
module G = GlobalGraph
module GB = GraphBuilder(G)

module K = Graph_api.Make(Capnp.BytesMessage)
let nt2nt (nt : node_type) cnt =
  let open K.Builder.Classification in
  match nt with
  | Root -> root_set cnt
  | LocalDef id -> local_def_set cnt
  | LocalDefType -> local_def_type_set cnt
  | LocalDefTerm -> local_def_term_set cnt
  | LocalAssum id -> local_assum_set cnt
  | Const c -> const_set cnt
  | ConstType -> const_type_set cnt
  | ConstUndef -> const_undef_set cnt
  | ConstDef -> const_def_set cnt
  | ConstOpaqueDef -> const_opaque_def_set cnt
  | ConstPrimitive -> const_primitive_set cnt
  | Ind i -> ind_set cnt
  | Construct c -> construct_set cnt
  | Proj p -> proj_set cnt
  | Sort -> sort_set cnt
  | SProp -> s_prop_set cnt
  | Prop -> prop_set cnt
  | Set -> set_set cnt
  | Type -> type_set cnt
  | Rel -> rel_set cnt
  | Var -> var_set cnt
  | Evar n -> evar_set cnt (Stdint.Uint64.of_int n)
  | EvarSubst -> evar_subst_set cnt
  | Cast -> cast_set cnt
  | CastTerm -> cast_term_set cnt
  | CastType -> cast_type_set cnt
  | Prod n -> prod_set cnt
  | ProdType -> prod_type_set cnt
  | ProdTerm -> prod_term_set cnt
  | Lambda n -> lambda_set cnt
  | LambdaType -> lambda_type_set cnt
  | LambdaTerm -> lambda_term_set cnt
  | LetIn n -> let_in_set cnt
  | LetInDef -> let_in_def_set cnt
  | LetInType -> let_in_type_set cnt
  | LetInTerm -> let_in_term_set cnt
  | App -> app_set cnt
  | AppFun -> app_fun_set cnt
  | AppArg -> app_arg_set cnt
  | Case -> case_set cnt
  | CaseTerm -> case_term_set cnt
  | CaseReturn -> case_return_set cnt
  | CaseBranch -> case_branch_set cnt
  | CBConstruct -> c_b_construct_set cnt
  | CBTerm -> c_b_term_set cnt
  | Fix -> fix_set cnt
  | FixFun n -> fix_fun_set cnt
  | FixFunType -> fix_fun_type_set cnt
  | FixFunTerm -> fix_fun_term_set cnt
  | FixReturn -> fix_return_set cnt
  | CoFix -> co_fix_set cnt
  | CoFixFun n -> co_fix_fun_set cnt
  | CoFixFunType -> co_fix_fun_type_set cnt
  | CoFixFunTerm -> co_fix_fun_term_set cnt
  | CoFixReturn -> co_fix_return_set cnt
  | Int i -> int_set cnt (Stdint.Uint64.of_int @@ snd @@ Uint63.to_int2 i)
  | Float f -> float_set cnt (float64_to_float f)
  | Primitive p -> primitive_set cnt (CPrimitives.to_string p)

let write_classification_list classification_arr nodes =
  let arr = classification_arr (List.length nodes) in
  List.iteri (fun i x ->
      let arri = Capnp.Array.get arr i in
      nt2nt x arri) nodes

let write_edge_list edge_arr edges =
  let open G in
  let arr = edge_arr (List.length @@ edges) in
  List.iteri (fun i { from; toward=(tp, ti) } ->
      let arri = Capnp.Array.get arr i in
      K.Builder.DirectedEdge.source_set_int_exn arri from;
      let toward = K.Builder.DirectedEdge.target_get arri in
      K.Builder.DirectedEdge.Target.dep_index_set_exn toward @@ tp;
      K.Builder.DirectedEdge.Target.node_index_set_int_exn toward ti
    ) edges

let write_graph capnp_graph nodes edges =
  let arr = K.Builder.Graph.classifications_init capnp_graph in
  write_classification_list arr nodes;
  let arr = K.Builder.Graph.edges_init capnp_graph in
  write_edge_list arr edges
