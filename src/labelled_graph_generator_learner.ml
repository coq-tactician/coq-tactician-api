open Tactician_ltac1_record_plugin
open Tactic_learner
open Names
open Labelled_graph_extractor
open Labelled_graph_def
open Labelled_graph_capnp_generator

let dirpath = Global.current_dirpath ()
let data_file = match Ltacrecord.base_filename with
  | None -> CErrors.anomaly Pp.(str "Source file location could not be found")
  | Some f -> f ^ ".bin"

module GraphGeneratorLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS

  module G = GlobalGraph
  module GB = GraphBuilder(G)

  type model = ((proof_state list * tactic) list * Libnames.full_path) list

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

  let dependencies = Summary.ref ~name:"TacticianGraphDependencies" DPmap.empty
  let in_dependencies : (DirPath.t * string) -> Libobject.obj =
    Libobject.(declare_object @@ superglobal_object "LTACRECORDGRAPHDEPS"
        ~cache:(fun (_, (path, bin)) -> dependencies := DPmap.add path bin !dependencies)
        ~subst:None
        ~discharge:(fun x -> Some (snd x)))

  let cache_type name =
    let dirp = Global.current_dirpath () in
    if Libnames.is_dirpath_prefix_of dirp (Libnames.dirpath name) then `File else `Dependency

  let empty () = []
  let learn db (path, _status) outcomes tac =
    match cache_type path with
    | `File ->
      let proof_states = List.map (fun x -> x.before) outcomes in
      let db = match db with
        | (ls, ppath)::data when Libnames.eq_full_path path ppath ->
          ((proof_states, tac)::ls, ppath)::data
        | _ -> ([proof_states, tac], path)::db in
      last_model := db; db
    | `Dependency -> db
  let predict db situations = IStream.empty
  let evaluate db _ _ = 0., db

  let resolve_dependencies deps =
    List.filter_map (fun p ->
        let f = match DPmap.find_opt p !dependencies with
          | None -> CErrors.anomaly (Pp.str ("Dependency was not resolvable: " ^ DirPath.to_string p))
          | Some f -> f in
        Feedback.msg_warning Pp.(str f);
        let f = CUnix.strip_path f in
        Feedback.msg_warning Pp.(str f);
        if Filename.is_relative f then Some f else None) deps

  module K = Labelled_graph_api.Make(Capnp.BytesMessage)
  let write_graph graph tactical_constants =
    let open G in
    let depslist = dirpath :: DPset.elements (DPset.remove dirpath graph.paths) in
    let path_index = CList.fold_left_i (fun i map path -> DPmap.add path i map) 0 DPmap.empty depslist in
    let resolved_dependencies = resolve_dependencies depslist in
    let g = K.Builder.Dataset.init_root () in
    let _ = K.Builder.Dataset.dependencies_set_list g resolved_dependencies in
    let _ = K.Builder.Dataset.tactical_definitions_set_list g @@
      List.map (fun n -> Stdint.Uint32.of_int @@ snd n) tactical_constants in
    let nodes = node_list graph in
    let edges = edge_list graph in
    let transformer tp =
      DPmap.find tp path_index in
    let capnp_graph = K.Builder.Dataset.graph_init g in
    write_graph capnp_graph transformer nodes edges;
    Capnp_unix.IO.write_message_to_file ~compression:`Packing (K.Builder.Dataset.to_message g) data_file

  open GB

  let proof_state_to_pair ps =
    let concl = proof_state_goal ps in
    let hyps = proof_state_hypotheses ps in
    (List.map (map_named term_repr) hyps, term_repr concl)

  let endline_hook () = print_endline "writing";
    let globrefs = Environ.Globals.view (Global.env ()).env_globals in
    let constants = Cset_env.elements @@ Cmap_env.domain @@ Cmap_env.filter
        (fun c _ -> not @@ Cmap.mem c !global_nodes.constants) globrefs.constants in
    (* We are only interested in canonical constants *)
    let constants = Cset.elements @@ OList.fold_left (fun m c ->
        let c = Constant.make1 @@ Constant.canonical c in
        Cset.add c m) Cset.empty constants in
    let minductives = Mindmap_env.Set.elements @@ Mindmap_env.domain @@ Mindmap_env.filter
        (fun m _ -> not @@ Indmap.mem (m, 0) !global_nodes.inductives) globrefs.inductives in
    (* We are only interested in canonical inductives *)
    let minductives = Mindset.elements @@ OList.fold_left (fun m c ->
        let c = MutInd.make1 @@ MutInd.canonical c in
        Mindset.add c m) Mindset.empty minductives in
    let open Tactician_util.WithMonadNotations(GB.M) in
    let open Monad.Make(GB.M) in

    let proof_states = OList.rev !last_model in
    let proof_states = OList.map (fun (prf, c) -> OList.rev prf, c) proof_states in

    (* Correctness check *)
    let lemmas = OList.map snd proof_states in
    let context_lemmas = OList.map (fun x -> Nametab.path_of_global (Names.GlobRef.ConstRef x)) constants in
    let lemmas = OList.filter (fun x -> not @@ OList.exists (fun y -> Libnames.eq_full_path x y ) context_lemmas) lemmas in
    OList.iter (fun x ->
        let base = Libnames.basename x in
        let candidates = OList.filter (fun x -> Names.Id.equal (Libnames.basename x) base ) context_lemmas in
        Feedback.msg_warning Pp.(str "Unknown lemma: " ++ Libnames.pr_path x ++ str "\nAlternatives:\n" ++
                                 prlist_with_sep (fun () -> str "\n") Libnames.pr_path candidates)) lemmas;
    (* End correctness check *)

    let proofs_map = OList.fold_left (fun m c ->
        let path = Nametab.path_of_global (Names.GlobRef.ConstRef c) in
        let proof = OList.find_opt (fun (p, path2) -> Libnames.eq_full_path path path2) proof_states in
        let proof = Option.map fst proof in
        let proof = Option.map (OList.map (fun (pss, tac) ->
            OList.map proof_state_to_pair pss, tactic_repr tac)) proof in
        Option.fold_left (fun m proof -> Cmap.add c proof m) m proof
      ) Cmap.empty constants in

    let updater =
      let+ cnodes = List.map (fun c ->
          let+ n = gen_const ContextSubject c in
        c, n) constants
      and+ () = List.iter gen_mutinductive_helper minductives in
      cnodes in
    let focus, graph = G.mk_node G.empty Root in (* This node is superfluous, but who cares *)
    let graph =
        { graph
        ; constants = !global_nodes.constants
        ; inductives = !global_nodes.inductives
        ; constructors = !global_nodes.constructors
        ; projections = !global_nodes.projections } in
    let context = { relative = []; named = Id.Map.empty; focus; initial_focus = focus
                  ; follow_defs = true; proofs_map } in
    let graph, constant_nodes = updater (graph, context) in
    let tactical_constants = OList.filter_map (fun (c, n) ->
        match Cmap.mem c proofs_map with
        | true -> Some n
        | false -> None) constant_nodes in
    let global_node =
      { constants = Cmap.filter
            (fun c _ -> not @@ Cmap.mem c !global_nodes.constants) graph.constants
      ; inductives = Indmap.filter
            (fun i _ -> not @@ Indmap.mem i !global_nodes.inductives) graph.inductives
      ; constructors = Constrmap.filter
            (fun c _ -> not @@ Constrmap.mem c !global_nodes.constructors) graph.constructors
      ; projections = ProjMap.filter
            (fun p _ -> not @@ ProjMap.mem p !global_nodes.projections) graph.projections } in
    Lib.add_anonymous_leaf @@ in_dependencies (dirpath, data_file);
    write_graph graph.graph tactical_constants;
    Lib.add_anonymous_leaf @@ in_global_nodes global_node

  let () = Declaremods.append_end_library_hook endline_hook
end

(* let () = register_online_learner "Dataset Generator Learner" (module GraphGeneratorLearner) *)
(* let () = Tactic_learner_internal.disable_queue () *)
