open Graph_extractor
open Graph_def

let order_option = Goptions.declare_bool_option_and_ref
      ~depr:false ~name:"order graph nodes"
      ~key:["Tactician"; "Reinforce"; "Visualize"; "Ordered"]
      ~value:true

let label_option = Goptions.declare_bool_option_and_ref
    ~depr:false ~name:"order graph nodes"
    ~key:["Tactician"; "Reinforce"; "Visualize"; "Labels"]
    ~value:false

type vertex = { tag : int; label : int node_type }
module G = Graph.Persistent.Digraph.ConcreteLabeled(
  struct
    type t = vertex
    let compare x y = Int.compare x.tag y.tag
    let hash x = Int.hash x.tag
    let equal x y = Int.equal x.tag y.tag
  end)
    (struct type t = edge_type let compare = compare let default = ContextSubject end)

module GraphvizGraph = struct
  include G

  let vertex_name x = string_of_int @@ (V.label x).tag

  let arrow_heads = [ `Dot; `Inv; `Odot; `Invdot; `Invodot ]

  let graph_attributes _ = if order_option () then [`OrderingOut] else []
  let default_vertex_attributes _ = []
  let vertex_attributes n = [`Label Graph_def.(show_node_type (fun _ _ -> ()) @@ (V.label n).label)]
  let default_edge_attributes _ = []
  let edge_attributes e =
    (if label_option () then [ `Label Graph_def.(show_edge_type @@ E.label e) ] else []) @
    [ `Dir `Both
    ; `Arrowtail (List.nth arrow_heads @@ edge_type_int_mod @@ E.label e)] @
    (match E.label e with
    | AppArgOrder | EvarSubstOrder -> [`Constraint false]
    | _ -> [`Constraint true])
  let get_subgraph _ = None

  let mk_edge g sort ~source ~target = add_edge_e g (E.create source sort target)
  let mk_node g label =
    let n = V.create { tag = nb_vertex g; label } in
    n, add_vertex g n
end

module Dot = Graph.Graphviz.Dot(GraphvizGraph)
module CICGraph = struct
  type node' = int
  include CICGraphMonad(SimpleGraph(
      struct
        type result = (node' node_type * (edge_type * int) list) DList.t
        type edge_label = edge_type
        type node_label = node' node_type end))
end
module Builder = GraphBuilder(CICGraph)

(* TODO: Probably not the most beautiful and efficient solution *)
let cic_graph_to_dot_graph ns =
  let ns = ns (fun ~node_count:_ ~edge_count:_ -> DList.nil) (fun ns nl ch -> DList.cons (nl, ch) ns) in
  let ns = DList.to_list ns in
  let nm, g = CList.fold_left_i (fun tag (nm, g) (label, _) ->
      let node = { tag; label } in
      Int.Map.add tag node nm, GraphvizGraph.add_vertex g node) 0 (Int.Map.empty, GraphvizGraph.empty) ns in
  CList.fold_left_i (fun tag g (_, children) ->
      let source = Int.Map.find tag nm in
      List.fold_left (fun g (label, tag) ->
          let target = Int.Map.find tag nm in
        GraphvizGraph.mk_edge g label ~source ~target) g children) 0 g ns

let make_graph graph =
  let graph = cic_graph_to_dot_graph graph in
  let chan = open_out "graph.dot" in
  Dot.output_graph chan graph;
  close_out chan;
  ignore @@ Sys.command "dot -Tpdf graph.dot -o graph.pdf"

let make_global_graph x follow_defs =
  let x =
    try
      Smartlocate.locate_global_with_alias x
    with Not_found -> CErrors.user_err (Pp.str "Invalid ident given") in
  let (_, _), ns = CICGraph.run_empty ~follow_defs Names.Cmap.empty @@ Builder.gen_globref x in
  make_graph ns

let make_constr_graph c follow_defs =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let evd, c = Constrintern.interp_constr_evars env sigma c in
  let (_, _), ns = CICGraph.run_empty ~follow_defs Names.Cmap.empty @@
    Builder.gen_constr (EConstr.to_constr evd c) in
  make_graph ns

let make_proof_graph state follow_defs =
  let _ =
    Pfedit.solve (Goal_select.get_default_goal_selector ()) None
      (Proofview.tclBIND Builder.gen_proof_state (fun res ->
           let (_, _), ns = CICGraph.run_empty ~follow_defs Names.Cmap.empty res in
           make_graph ns;
           Proofview.tclUNIT ()))
      (Proof_global.get_proof state) in ()
