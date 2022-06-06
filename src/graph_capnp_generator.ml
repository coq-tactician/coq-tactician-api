open Graph_def
open Names

module K = Graph_api.Make(Capnp.BytesMessage)

type graph_state = { node_index : int; edge_index : int }

module CapnpGraphWriter(P : sig type path end)(G : GraphMonadType with type node = P.path * (bool * int)) = struct

  let nt2nt ~include_metadata none_index transformer node_index_transform (nt : G.node node_type) cnt =
    let open K.Builder.Graph.Node.Label in
    match nt with
    | ProofState -> proof_state_set cnt
    | UndefProofState -> undef_proof_state_set cnt
    | ContextDef id -> context_def_set cnt (if include_metadata then Id.to_string id else "")
    | ContextAssum id -> context_assum_set cnt (if include_metadata then Id.to_string id else "")
    | Definition { previous; external_previous; def_type; path; status; type_text; term_text } ->
      let cdef = definition_init cnt in
      let open K.Builder.Definition in
      (* TODO: We should probably derive the hash of a constant form its path (until we obtain actual hashes) *)
      name_set cdef ((match def_type with | Proj _ -> "Projection:" | _ -> "") ^ Libnames.string_of_path path);
      if include_metadata then begin
          type_text_set cdef type_text;
          term_text_set cdef @@ Option.default "" term_text
        end;
      let capnp_status = status_init cdef in
      (match status with
       | DOriginal -> Status.original_set capnp_status
       | DDischarged (dep, index) ->
         if transformer dep <> 0 then
           CErrors.anomaly Pp.(str "discharged definition was not from the current file");
         Status.discharged_set_int_exn capnp_status @@ node_index_transform index
       | DSubstituted (dep, index) ->
         let capnp_node = Status.substituted_init capnp_status in
         Status.Substituted.dep_index_set_exn capnp_node @@ transformer dep;
         Status.Substituted.node_index_set_int_exn capnp_node @@ node_index_transform index);
      let write_proof arr proof =
        let write_proof_state capnp_state { root; context; ps_string; evar } =
          K.Builder.ProofState.root_set_int_exn capnp_state @@ node_index_transform @@ snd root;
          let _ = K.Builder.ProofState.context_set_list capnp_state
              (List.map (fun x -> Stdint.Uint32.of_int @@ node_index_transform @@ snd x) context) in
          if include_metadata then
            K.Builder.ProofState.text_set capnp_state ps_string;
          K.Builder.ProofState.id_set_int_exn capnp_state @@ Evar.repr evar in
        let write_outcome capnp { term; term_text; arguments; proof_state_before; proof_states_after } =
          let capnp_state_before = K.Builder.Outcome.before_init capnp in
          write_proof_state capnp_state_before proof_state_before;
          let after_arr = K.Builder.Outcome.after_init capnp (List.length proof_states_after) in
          List.iteri (fun i ps ->
              let capnp_state = Capnp.Array.get after_arr i in
              write_proof_state capnp_state ps;
            ) proof_states_after;
          K.Builder.Outcome.term_set_int_exn capnp @@ node_index_transform @@ snd term;
          if include_metadata then
            K.Builder.Outcome.term_text_set capnp @@ term_text;
          let arg_arr = K.Builder.Outcome.tactic_arguments_init capnp (List.length arguments) in
          List.iteri (fun i arg ->
              let arri = Capnp.Array.get arg_arr i in
              match arg with
              | None -> K.Builder.Argument.unresolvable_set arri
              | Some (dep, index) ->
                let node = K.Builder.Argument.term_init arri in
                K.Builder.Argument.Term.dep_index_set_exn node @@ transformer dep;
                K.Builder.Argument.Term.node_index_set_int_exn node @@ node_index_transform index
            ) arguments in
        List.iteri (fun i { tactic; outcomes } ->
            let arri = Capnp.Array.get arr i in
            let capnp_tactic = K.Builder.ProofStep.tactic_init arri in
            (match tactic with
             | None -> K.Builder.ProofStep.Tactic.unknown_set capnp_tactic
             | Some { tactic; base_tactic; interm_tactic; tactic_hash; tactic_exact } ->
               let capnp_tactic = K.Builder.ProofStep.Tactic.known_init capnp_tactic in
               K.Builder.Tactic.ident_set_int_exn capnp_tactic tactic_hash;
               K.Builder.Tactic.exact_set capnp_tactic tactic_exact;
               if include_metadata then begin
                 K.Builder.Tactic.text_set capnp_tactic tactic;
                 K.Builder.Tactic.base_text_set capnp_tactic base_tactic;
                 K.Builder.Tactic.interm_text_set capnp_tactic interm_tactic
               end);
            let outcome_arr = K.Builder.ProofStep.outcomes_init arri (List.length outcomes) in
            List.iteri (fun i outcome ->
                let outcome_arri = Capnp.Array.get outcome_arr i in
                write_outcome outcome_arri outcome
              ) outcomes
          ) proof in
      (match previous with
       | None -> previous_set_int_exn cdef none_index;
       | Some previous ->
         if transformer (fst previous) <> 0 then
           CErrors.anomaly Pp.(str "previous definition was not from the current file");
         previous_set_int_exn cdef @@ node_index_transform @@ snd previous);
      ignore (external_previous_set_list cdef @@ List.map (fun x -> transformer (fst x)) external_previous);
      (match def_type with
       | TacticalConstant (c, proof) ->
         let hash = Constant.UserOrd.hash c in
         hash_set cdef (Stdint.Uint64.of_int hash);
         let arr = tactical_constant_init cdef (List.length proof) in
         write_proof arr proof
       | TacticalSectionConstant (id, proof) ->
         let hash = Id.hash id in
         hash_set cdef (Stdint.Uint64.of_int hash);
         let arr = tactical_section_constant_init cdef (List.length proof) in
         write_proof arr proof
       | ManualConst c ->
         let hash = Constant.UserOrd.hash c in
         hash_set cdef (Stdint.Uint64.of_int hash);
         manual_constant_set cdef
       | ManualSectionConst id ->
         let hash = Id.hash id in
         hash_set cdef (Stdint.Uint64.of_int hash);
         manual_section_constant_set cdef
       | Ind (m, i) ->
         let hash = Hashtbl.hash (i, MutInd.UserOrd.hash m) in
         hash_set cdef (Stdint.Uint64.of_int hash);
         inductive_set cdef
       | Construct ((m, i), j) ->
         let hash = Hashtbl.hash (i, j, MutInd.UserOrd.hash m) in
         hash_set cdef (Stdint.Uint64.of_int @@ hash);
         constructor_set cdef
       | Proj p ->
         let hash = Projection.Repr.UserOrd.hash p in
         hash_set cdef @@ Stdint.Uint64.of_int hash; (* TODO: This is probably not a good hash *)
         projection_set cdef
      )
    | ConstEmpty -> const_empty_set cnt
    | SortSProp -> sort_s_prop_set cnt
    | SortProp -> sort_prop_set cnt
    | SortSet -> sort_set_set cnt
    | SortType -> sort_type_set cnt
    | Rel -> rel_set cnt
    | Evar -> evar_set cnt
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
    | Int i ->
      let p = int_init cnt in
      K.Builder.IntP.value_set p @@ Stdint.Uint64.of_int @@ snd @@ Uint63.to_int2 i
    | Float f ->
      let p = float_init cnt in
      K.Builder.FloatP.value_set p @@ float64_to_float f
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
    | EvarSubstPointer -> EvarSubstPointer
    | EvarSubstTerm -> EvarSubstTerm
    | EvarSubstTarget -> EvarSubstTarget
    | EvarSubject -> EvarSubject

  let write_graph ?(include_metadata=false) capnp_graph transformer node_index_transform
      node_count edge_count nodes =
    let cnodes = K.Builder.Graph.nodes_init capnp_graph node_count in
    let edges = K.Builder.Graph.edges_init capnp_graph edge_count in
    let state = { node_index = 0; edge_index = 0 } in
    let _ = AList.fold (fun { node_index; edge_index } (label, children) ->
        let node = Capnp.Array.get cnodes node_index in
        nt2nt ~include_metadata node_count transformer node_index_transform label @@
        K.Builder.Graph.Node.label_init node;
        let cc = List.length children in
        K.Builder.Graph.Node.children_count_set_exn node cc;
        K.Builder.Graph.Node.children_index_set_int_exn node (if cc = 0 then 0 else edge_index);
        let edge_index = List.fold_left (fun ei (label, (tp, ti)) ->
            let et = Capnp.Array.get edges ei in
            K.Builder.Graph.EdgeTarget.label_set et @@ et2et label;
            let ctarget = K.Builder.Graph.EdgeTarget.target_init et in
            K.Builder.Graph.EdgeTarget.Target.dep_index_set_exn ctarget @@ transformer tp;
            K.Builder.Graph.EdgeTarget.Target.node_index_set_int_exn ctarget @@ node_index_transform ti;
            ei + 1
          ) edge_index children in
        { node_index = node_index + 1; edge_index })
        nodes state in
    ()
end
