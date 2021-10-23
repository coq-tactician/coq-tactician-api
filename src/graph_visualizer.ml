open Graph_extractor
open Graph_def

module G = Graph.Persistent.Digraph.Abstract(struct type t = node_type end)

module GraphvizGraph = struct
  include G

  let vertex_name x = string_of_int @@ V.hash x

  let graph_attributes _ = if Labelled_graph_visualizer.order_option () then [`OrderingOut] else []
  let default_vertex_attributes _ = []
  let vertex_attributes n = [`Label Graph_def.(show_node_type @@ V.label n)]
  let default_edge_attributes _ = []
  let edge_attributes e =
    (match V.label @@ E.src e, V.label @@ E.dst e with
     | AppFun, AppArg | AppArg, AppFun -> [`Constraint false]
     | _ -> [`Constraint true])
  let get_subgraph _ = None

  type node = V.t
  let mk_edge g ~from ~toward = add_edge g from toward
  let mk_node g typ =
    let n = V.create typ in
    n, add_vertex g n
end

module Dot = Graph.Graphviz.Dot(GraphvizGraph)
module Builder = GraphBuilder(GraphvizGraph)

let make_graph state =
  let graph = Builder.(state.graph) in
  let chan = open_out "graph.dot" in
  Dot.output_graph chan graph;
  close_out chan;
  ignore @@ Sys.command "dot -Tpdf graph.dot -o graph.pdf"

let make_global_graph x follow =
  let x =
    try
      Smartlocate.locate_global_with_alias x
    with Not_found -> CErrors.user_err (Pp.str "Invalid ident given") in
  let state, _n = Builder.run_empty follow @@ Builder.gen_globref x in
  make_graph state

let make_constr_graph c follow =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let evd, c = Constrintern.interp_constr_evars env sigma c in
  let state, () = Builder.run_empty follow @@ Builder.gen_constr (EConstr.to_constr evd c) in
  make_graph state

let make_proof_graph state follow =
  let _ =
    Pfedit.solve (Goal_select.get_default_goal_selector ()) None
      (Proofview.tclBIND Builder.gen_proof_state (fun res ->
           let state, () = Builder.run_empty follow res in
           make_graph state;
           Proofview.tclUNIT ()))
      (Proof_global.get_proof state) in ()
