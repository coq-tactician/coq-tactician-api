open Dag_def
open Dag_extractor
open Names

let dirpath = Global.current_dirpath ()
module GlobalGraph = struct
  type node = DirPath.t * int
  type t =
    { nodes : (node_type * node list) list
    ; paths : DPset.t
    ; last  : int }
  let empty = { nodes = []; paths = DPset.empty; last = 0 }
  let index_to_node i = dirpath, i
  let mk_node { nodes; paths; last } typ children =
    let paths = List.fold_left (fun paths (tp, ti) -> DPset.add tp paths) paths children in
    index_to_node last, { nodes = (typ, children)::nodes; paths; last = last + 1}
  let node_list { nodes; _ } = List.rev nodes
end
module G = GlobalGraph
module GB = DAGBuilder(G)

module K = Dag_api.Make(Capnp.BytesMessage)
let mk_node_cn (nt : node_type) children cn =
  let open K.Builder.Dag.Node in
  let open K.Builder.Dag.LeafKind in
  let open K.Builder.Dag.Node.Node in
  let open K.Builder.Dag.NodeRef in
  let mk_leaf lt =
    let leaf = leaf_init cn in
    lt leaf in
  let mk_node' cn lt =
    let node = node_init cn in
    kind_set node lt;
    let arr = children_init node (List.length children) in
    List.iteri (fun i (tp, ti) ->
        let arri = Capnp.Array.get arr i in
        dep_index_set_exn arri tp;
        node_index_set_int_exn arri ti
      ) children in
  let mk_node lt = mk_node' cn lt in
  let mk_definition name lt =
    let name = Hashtbl.hash @@ Libnames.string_of_path @@ Nametab.path_of_global name in
    mk_leaf (fun x ->
        let def = definition_init x in
        Definition.name_hash_set_int_exn def name;
        let nd = Definition.node_init def in
        mk_node' nd lt)
  in
  match nt with
  | WithContext -> mk_node WithContext
  | Context -> mk_node Context
  | ContextDef id -> mk_node ContextDef
  | ContextAssum id -> mk_node ContextAssum
  | ConstUndef c -> mk_definition (GlobRef.ConstRef c) ConstUndef
  | ConstDef c -> mk_definition (GlobRef.ConstRef c) ConstDef
  | ConstOpaqueDef c -> mk_definition (GlobRef.ConstRef c) ConstOpaqueDef
  | Ind i -> mk_definition (GlobRef.IndRef i) Ind
  | IndBinder -> mk_node IndBinder
  | IndConstructs -> mk_node IndConstructs
  | Construct c -> mk_definition (GlobRef.ConstructRef c) Construct
  | Proj p -> mk_definition (GlobRef.ConstRef (Projection.Repr.constant p)) Proj
  | Evar -> mk_node Evar
  | EvarSubsts -> mk_node EvarSubsts
  | Cast -> mk_node Cast
  | Prod n -> mk_node Prod
  | ProdBinder -> mk_node ProdBinder
  | Lambda n -> mk_node Lambda
  | LambdaBinder -> mk_node LambdaBinder
  | LetIn n -> mk_node LetIn
  | LetInBinder -> mk_node LetInBinder
  | App -> mk_node App
  | AppArgs -> mk_node AppArgs
  | Case -> mk_node Case
  | CaseBranches -> mk_node CaseBranches
  | CaseBranch -> mk_node CaseBranch
  | Fix -> mk_node Fix
  | FixFun n -> mk_node FixFun
  | FixAuxes -> mk_node FixAuxes
  | FixFunBinder -> mk_node FixFunBinder
  | CoFix -> mk_node CoFix
  | CoFixAuxes -> mk_node CoFixAuxes
  | CoFixFun n -> mk_node CoFixFun
  | CoFixFunBinder -> mk_node CoFixFunBinder

  | Int i -> mk_leaf (fun x -> int_set x (Stdint.Uint64.of_int @@ snd @@ Uint63.to_int2 i))
  | Float f -> mk_leaf (fun x -> float_set x (float64_to_float f))
  | SortSProp -> mk_leaf sort_s_prop_set
  | SortProp -> mk_leaf sort_prop_set
  | SortSet -> mk_leaf sort_set_set
  | SortType -> mk_leaf sort_type_set
  | ConstPrimitive c -> mk_leaf (fun x -> const_primitive_set x (CPrimitives.to_string c))
  | EvarId i -> mk_leaf (fun x -> evar_id_set_int_exn x i)

let write_node_list node_arr nodes =
  let arr = node_arr (List.length nodes) in
  List.iteri (fun i (nt, children) ->
      let arri = Capnp.Array.get arr i in
      mk_node_cn nt children arri) nodes
