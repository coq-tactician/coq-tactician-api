open Tactician_ltac1_record_plugin
open Tactic_learner
open Names
open Labelled_graph_extractor
open Labelled_graph_def
open Labelled_graph_capnp_generator

let dirpath = Global.current_dirpath ()
let data_file = Option.default "" Ltacrecord.base_filename ^ ".bin"

module GraphGeneratorLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

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

  let cache_type name =
    let dirp = Global.current_dirpath () in
    if Libnames.is_dirpath_prefix_of dirp (Libnames.dirpath name) then `File else `Dependency

  let empty () = []
  let learn db (name, _status) outcomes tac =
    match cache_type name with
    | `File ->
      let db = List.map (fun outcome -> outcome.before, tac) outcomes @ db in
      last_model := db; db
    | `Dependency -> db
  let predict db situations = IStream.empty
  let evaluate db _ _ = 0., db

  module K = Graph_api.Make(Capnp.BytesMessage)
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
    let edges = edge_list graph in
    let edges = List.rev @@ List.rev_map (fun { from; sort; toward=(tp, ti) } ->
        { from; sort; toward = (DPmap.find tp path_index, ti) }
      ) edges in
    let capnp_graph = K.Builder.Dataset.graph_init g in
    write_graph capnp_graph nodes edges;
    let arr = K.Builder.Dataset.proof_steps_init g (List.length proof_states) in
    List.iteri (fun i (node, context, tactic, args) ->
        let arri = Capnp.Array.get arr i in
        let state = K.Builder.Dataset.DataPoint.state_init arri in
        let capnp_tactic = K.Builder.Dataset.DataPoint.tactic_init arri in
        K.Builder.ProofState.root_set_int_exn state node;
        let _ = K.Builder.ProofState.context_set_list state
            (List.map Stdint.Uint32.of_int context) in
        K.Builder.Tactic.ident_set_int_exn capnp_tactic @@ tactic_hash tactic;
        let _ = K.Builder.Tactic.arguments_set_list capnp_tactic
            (List.map (fun arg -> Stdint.Uint32.of_int (Option.default 0 arg)) args) in
        ()
      ) proof_states;
    Capnp_unix.IO.write_message_to_file ~compression:`Packing (K.Builder.Dataset.to_message g) data_file

  open GB
  let gen_proof_state ps =
    let open M in
    let concl = proof_state_goal ps in
    let hyps = proof_state_hypotheses ps in
    with_named_context (List.map (map_named term_repr) hyps) (
      gen_constr ContextSubject (term_repr concl) >>
      map (fun c -> c.named) ask)

  let endline_hook () = print_endline "writing";
    let globrefs = Environ.Globals.view (Global.env ()).env_globals in
    let constants = Cset_env.elements @@ Cmap_env.domain @@ Cmap_env.filter
        (fun c _ -> not @@ Cmap.mem c !global_nodes.constants) globrefs.constants in
    let minductives = Mindmap_env.Set.elements @@ Mindmap_env.domain @@ Mindmap_env.filter
        (fun m _ -> not @@ Indmap.mem (m, 0) !global_nodes.inductives) globrefs.inductives in
    let open Tactician_util.WithMonadNotations(GB.M) in
    let open Monad.Make(GB.M) in
    let proof_states = OList.rev !last_model in
    let updater =
      List.iter (gen_const ContextSubject) constants >>
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
          let args = OList.map (fun (id, _) -> safe_index0 Names.Id.equal id context_dom) args in
          return (snd root, context_range, tac, args))
        proof_states in
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
