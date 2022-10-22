open Graph_extractor
open Graph_def

let order_option = Goptions.declare_bool_option_and_ref
      ~depr:false ~name:"order graph nodes"
      ~key:["Tactician"; "Neural"; "Visualize"; "Ordered"]
      ~value:true

let label_option = Goptions.declare_bool_option_and_ref
    ~depr:false ~name:"order graph nodes"
    ~key:["Tactician"; "Neural"; "Visualize"; "Labels"]
    ~value:false

let hash_option = Goptions.declare_bool_option_and_ref
    ~depr:false ~name:"order graph nodes"
    ~key:["Tactician"; "Neural"; "Visualize"; "Hashes"]
    ~value:false

type vertex = { tag : int; label : string }
module G = Graph.Persistent.Digraph.ConcreteLabeled(
  struct
    type t = vertex
    let compare x y = Int.compare x.tag y.tag
    let hash x = Int.hash x.tag
    let equal x y = Int.equal x.tag y.tag
  end)(
  struct type t = edge_type
    let compare = compare
    let default = ContextSubject
  end)

module GraphvizGraph = struct
  include G

  let vertex_name x = string_of_int @@ (V.label x).tag

  let arrow_heads = [ `Dot; `Inv; `Odot; `Invdot; `Invodot ]

  let graph_attributes _ = if order_option () then [`OrderingOut] else []
  let default_vertex_attributes _ = []
  let vertex_attributes n = [`Label (V.label n).label]
  let default_edge_attributes _ = []
  let edge_attributes e =
    (if label_option () then [ `Label Graph_def.(show_edge_type @@ E.label e) ] else []) @
    [ `Dir `Both
    ; `Arrowtail (List.nth arrow_heads @@ edge_type_int_mod @@ E.label e)]
  let get_subgraph _ = None

  let mk_edge g sort ~source ~target = add_edge_e g (E.create source sort target)
  let mk_node g label =
    let n = V.create { tag = nb_vertex g; label } in
    n, add_vertex g n
end

module Dot = Graph.Graphviz.Dot(GraphvizGraph)

(* TODO: Probably not the most beautiful and efficient solution *)
let cic_graph_to_dot_graph transform ns =
  let ns = ns (fun ~node_count:_ ~edge_count:_ -> DList.nil) (fun ns nl ch -> DList.cons (nl, ch) ns) in
  let ns = DList.to_list ns in
  let nm, g = CList.fold_left_i (fun tag (nm, g) (label, _) ->
      let node = { tag; label = transform label } in
      Int.Map.add tag node nm, GraphvizGraph.add_vertex g node) 0 (Int.Map.empty, GraphvizGraph.empty) ns in
  CList.fold_left_i (fun tag g (_, children) ->
      let source = Int.Map.find tag nm in
      List.fold_left (fun g (label, tag) ->
          let target = Int.Map.find tag nm in
        GraphvizGraph.mk_edge g label ~source ~target) g children) 0 g ns

let make_graph transform graph =
  let graph = cic_graph_to_dot_graph transform graph in
  let chan = open_out "graph.dot" in
  Dot.output_graph chan graph;
  close_out chan;
  ignore @@ Sys.command "dot -Tpdf graph.dot -o graph.pdf"

module Viz
    (G : sig
       type node'
       type final
       val final_to_string : final -> string
       type result = (final * (edge_type * int) list) DList.t
       include GraphMonadType
         with type node = node'
          and type edge_label = edge_type
          and type node_label = node' node_type
          and type 'a repr_t =
                'a *
                ((node_count:int -> edge_count:int -> result) ->
                 (result -> final -> (edge_label * int) list -> result) ->
                 result)
     end) =
struct
  module GM = struct include CICGraphMonad(G) type node' = node end
  module Builder = GraphBuilder(GM)

  let make_global_graph ?def_depth x =
    let x =
      try
        Smartlocate.locate_global_with_alias x
      with Not_found -> CErrors.user_err (Pp.str "Invalid ident given") in
    let (_, _), ns = GM.run_empty ?def_depth
      (Builder.gen_globref (Global.env ()) (Names.Id.Map.empty, Names.Cmap.empty) x) in
    make_graph G.final_to_string ns

  let make_constr_graph ?def_depth c =
    let env = Global.env () in
    let sigma = Evd.from_env env in
    let evd, c = Constrintern.interp_constr_evars env sigma c in
    let (_, _), ns = GM.run_empty ?def_depth
      (Builder.gen_constr (Global.env ()) (Names.Id.Map.empty, Names.Cmap.empty) (EConstr.to_constr evd c)) in
    make_graph G.final_to_string ns

  let make_proof_graph ?def_depth state =
    let _ =
      Pfedit.solve (Goal_select.get_default_goal_selector ()) None
        (Proofview.tclBIND (Builder.gen_proof_state (Names.Id.Map.empty, Names.Cmap.empty)) (fun res ->
             let (_, _), ns = GM.run_empty ?def_depth res in
             make_graph G.final_to_string ns;
             Proofview.tclUNIT ()))
        (Proof_global.get_proof state) in ()

  end

module SimpleCICGraph = struct
  type final = int node_type
  type node' = int
  type result = (final * (edge_type * int) list) DList.t
  let final_to_string = Graph_def.show_node_type (fun _ _ -> ())
  include SimpleGraph(
    struct
      type nonrec result = (final * (edge_type * int) list) DList.t
      type edge_label = edge_type
      type node_label = final
    end)
  type 'a repr_t =
    'a *
    ((node_count:int -> edge_count:int -> result) ->
     (result -> final -> (edge_label * int) list -> result) ->
     result)
end

module rec SimpleHashedCICGraph : sig
  type final = SimpleHashedCICGraph.node node_type * int64
  type node' = SimpleHashedCICGraph.node
  type result = (final * (edge_type * int) list) DList.t
  val final_to_string : final -> string
  include GraphMonadType
    with type node_label = node' node_type
     and type edge_label = edge_type
     and type 'a repr_t =
           'a *
           ((node_count:int -> edge_count:int -> result) ->
            (result -> final -> (edge_label * int) list -> result) ->
            result)
end = struct
  type final = SimpleHashedCICGraph.node node_type * int64
  type node' = SimpleHashedCICGraph.node
  type result = (final * (edge_type * int) list) DList.t
  let final_to_string (nl, h) =
    let hash = if hash_option () then " : " ^ Int64.to_string h else "" in
    Graph_def.show_node_type (fun _ _ -> ()) nl ^ hash

  module rec Hasher :
    CICHasherType with type t = int64
                   and type node_label = node' node_type
                   and type edge_label = edge_label =
    CICHasher(XXHasher)
      (struct
        type node = node'
        let node_hash _ = XXHasher.with_state (fun s -> s)
      end)
  and HashMap : Map.S with type key = Hasher.t =
    Map.Make(Hasher)
  and GH : sig
    include GraphMonadType
      with type node_label = node' node_type
       and type edge_label = edge_type
       and type 'a repr_t =
             int HashMap.t ->
             (int HashMap.t * 'a) *
             ((node_count:int -> edge_count:int -> result) ->
              (result -> final -> (edge_label * int) list -> result) ->
              result)
  end
    = GraphHasher
      (struct
        type node_label = SimpleHashedCICGraph.node node_type
        type edge_label = edge_type
      end)
      (Hasher)(HashMap)
      (SimpleGraph(
        struct
          type nonrec result = result
          type edge_label = edge_type
          type node_label = final
        end))
  include GH
  type 'a repr_t =
    'a *
    ((node_count:int -> edge_count:int -> result) ->
     (result -> final -> (edge_label * int) list -> result) -> result)
  let run m = let (_, res), it = run m HashMap.empty in res, it
end

module SimpleViz = Viz(SimpleCICGraph)
module SimpleHashedViz = Viz(SimpleHashedCICGraph)
