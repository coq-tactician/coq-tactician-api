open Tactician_ltac1_record_plugin
open Tactic_learner
open Names
open Graph_extractor
open Graph_def
open Ltac_plugin

let dirpath = Global.current_dirpath ()
let data_file = Option.default "" Ltacrecord.base_filename ^ ".bin"

module GraphGeneratorLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

  module GlobalGraph = struct
    type node = DirPath.t * int
    type directed_edge =
      { from : int
      ; toward : node }
    type t =
      { nodes : node_type list
      ; paths : DPset.t
      ; last  : int
      ; assoc : directed_edge list }
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
    let edge_list { assoc; _ } = assoc (* No need to reverse this *)
  end
  module G = GlobalGraph
  module GB = GraphBuilder(G)

  type model = (proof_state * tactic) list

  let last_model = Summary.ref ~name:"dataset-generator-learner-lastmodel" []

  type global_nodes =
    { constants     : G.node Cmap.t
    ; inductives    : G.node Indmap.t
    ; constructors  : G.node Constrmap.t
    ; projections   : G.node GB.ProjMap.t }
  let merge_global_nodes gn1 gn2 =
    let mexn _ _ _ = CErrors.anomaly (Pp.str "Duplicate global nodes found") in
    { constants = Cmap.union mexn gn1.constants gn2.constants
    ; inductives = Indmap.union mexn gn1.inductives gn2.inductives
    ; constructors = Constrmap.union mexn gn1.constructors gn2.constructors
    ; projections = GB.ProjMap.union mexn gn1.projections gn2.projections }
  let global_nodes = Summary.ref ~name:"TacticianGraphGlobalNodes"
      { constants = Cmap.empty
      ; inductives = Indmap.empty
      ; constructors = Constrmap.empty
      ; projections = GB.ProjMap.empty }
  let in_global_nodes : global_nodes -> Libobject.obj =
    Libobject.(declare_object @@ superglobal_object "LTACRECORDGRAPHNODES"
                 ~cache:(fun (_, gn') -> global_nodes := merge_global_nodes !global_nodes gn')
                 ~subst:None
                 ~discharge:(fun x -> Some (snd x)))

  module StringSet = CSet.Make(String)
  let dependencies = Summary.ref ~name:"TacticianGraphDependencies" StringSet.empty
  let in_dependencies : string -> Libobject.obj =
    Libobject.(declare_object @@ superglobal_object "LTACRECORDGRAPHDEPS"
        ~cache:(fun (_, dep) -> dependencies := StringSet.add dep !dependencies)
        ~subst:None
        ~discharge:(fun x -> Some (snd x)))

let cache_type n =
  let dirp = Global.current_dirpath () in
  if Libnames.is_dirpath_prefix_of dirp (Names.ModPath.dp @@ fst @@ Names.Constant.repr2 n) then `File else `Dependency

  let empty () = []
  let learn db loc outcomes tac =
    match cache_type loc with
    | `File ->
      let db = List.map (fun outcome -> outcome.before, tac) outcomes @ db in
      last_model := db; db
    | `Dependency -> db
  let predict db situations = IStream.empty
  let evaluate db _ _ = 0., db

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
  let write_graph graph proof_states =
    let open G in
    let depslist = dirpath :: DPset.elements (DPset.remove dirpath graph.paths) in
    let path_index = CList.fold_left_i (fun i map path -> DPmap.add path i map) 0 DPmap.empty depslist in
    let relativized_dependencies =
      List.filter_map (fun p ->
          let f = match Ltacrecord.try_locate_absolute_library p with
            | None -> CErrors.user_err (Pp.str ("Path was not locatable: " ^ DirPath.to_string p))
            | Some f -> f in
          let f = CUnix.strip_path f in
          if Filename.is_relative f then Some f else None) depslist in
    let g = K.Builder.Dataset.init_root () in
    let _ = K.Builder.Dataset.dependencies_set_list g relativized_dependencies in
    let nodes = node_list graph in
    let capnp_graph = K.Builder.Dataset.graph_init g in
    let arr = K.Builder.Graph.classifications_init capnp_graph (List.length nodes) in
    List.iteri (fun i x ->
        let arri = Capnp.Array.get arr i in
        nt2nt x arri) nodes;
    let arr = K.Builder.Graph.edges_init capnp_graph (List.length @@ edge_list graph) in
    List.iteri (fun i { from; toward=(tp, ti) } ->
        let arri = Capnp.Array.get arr i in
        K.Builder.DirectedEdge.source_set_int_exn arri from;
        let toward = K.Builder.DirectedEdge.target_get arri in
        K.Builder.DirectedEdge.Target.dep_index_set_exn toward @@ DPmap.find tp path_index;
        K.Builder.DirectedEdge.Target.node_index_set_int_exn toward ti
      ) (edge_list graph);
    let arr = K.Builder.Dataset.proof_steps_init g (List.length proof_states) in
    List.iteri (fun i (node, context, tactic, args) ->
        let arri = Capnp.Array.get arr i in
        let state = K.Builder.Dataset.DataPoint.state_init arri in
        let capnp_tactic = K.Builder.Dataset.DataPoint.tactic_init arri in
        K.Builder.ProofState.root_set_int_exn state node;
        let _ = K.Builder.ProofState.context_set_list state
          (List.map Stdint.Uint32.of_int context) in
        K.Builder.Tactic.id_set_int_exn capnp_tactic @@ tactic_hash tactic;
        let _ = K.Builder.Tactic.arguments_set_list capnp_tactic
          (List.map (fun arg -> (Option.default 0 arg)) args) in
        ()
      ) proof_states;
    Capnp_unix.IO.write_message_to_file ~compression:`Packing (K.Builder.Dataset.to_message g) data_file

  open GB
  let gen_proof_state ps =
    let open M in
    let concl = proof_state_goal ps in
    let hyps = proof_state_hypotheses ps in
    with_named_context' (List.map (map_named term_repr) hyps) (
      gen_constr (term_repr concl) >>
      map (fun c -> c.named) ask)

  let endline_hook () = print_endline "writing";
    let globrefs = Environ.Globals.view (Global.env ()).env_globals in
    let constants = Cset_env.elements @@ Cmap_env.domain @@ Cmap_env.filter
        (fun c _ -> not @@ Cmap.mem c !global_nodes.constants) globrefs.constants in
    let minductives = Mindmap_env.Set.elements @@ Mindmap_env.domain @@ Mindmap_env.filter
        (fun m _ -> not @@ Indmap.mem (m, 0) !global_nodes.inductives) globrefs.inductives in
    let open Tactician_util.WithMonadNotations(GB.M) in
    let open Monad.Make(GB.M) in
    let updater =
      List.iter gen_const constants >>
      List.iter gen_mutinductive_helper minductives >>
      List.map (fun (ps, tac) ->
          let* root = mk_node Root in
          let* context_map = with_focus root @@ gen_proof_state ps in
          let tac = tactic_repr tac in
          let tac = Tactic_normalize.tactic_strict tac in
          let args, tac = Tactic_abstract.tactic_abstract tac in
          let tac = tactic_make tac in
          let context = Id.Map.bindings context_map in
          let context_dom = OList.map fst context in
          let context_range = OList.map (fun (_, (_, n)) -> n) context in
          let safe_index0 f x l = try Some (CList.index0 f x l) with Not_found -> None in
          let args = OList.map (fun id -> safe_index0 Names.Id.equal id context_dom) args in
          return (snd root, context_range, tac, args))
        !last_model in
    let focus, graph = G.mk_node G.empty Root in (* This node is superfluous, but who cares *)
    let graph =
        { graph
        ; constants = !global_nodes.constants
        ; inductives = !global_nodes.inductives
        ; constructors = !global_nodes.constructors
        ; projections = !global_nodes.projections } in
    let context = { relative = []; named = Id.Map.empty; focus; follow_defs = true } in
    let graph, proof_states = updater (graph, context) in
    let global_node =
      { constants = Cmap.filter
            (fun c _ -> not @@ Cmap.mem c !global_nodes.constants) graph.constants
      ; inductives = Indmap.filter
            (fun i _ -> not @@ Indmap.mem i !global_nodes.inductives) graph.inductives
      ; constructors = Constrmap.filter
            (fun c _ -> not @@ Constrmap.mem c !global_nodes.constructors) graph.constructors
      ; projections = ProjMap.filter
            (fun p _ -> not @@ ProjMap.mem p !global_nodes.projections) graph.projections } in
    write_graph graph.graph proof_states;
    Lib.add_anonymous_leaf @@ in_global_nodes global_node;
    Lib.add_anonymous_leaf @@ in_dependencies data_file

  let () = Declaremods.append_end_library_hook endline_hook
end

(* let () = register_online_learner "Dataset Generator Learner" (module GraphGeneratorLearner) *)
(* let () = Tactic_learner_internal.disable_queue () *)
