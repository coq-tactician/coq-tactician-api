open Names
open Declarations
open Tactician_ltac1_record_plugin

let print_mutind fmt m =
  Format.fprintf fmt "%s" @@ MutInd.to_string m

let inductive_to_string i =
  let _, i = Global.lookup_inductive i in
  Id.to_string i.mind_typename

let print_inductive fmt i =
  Format.fprintf fmt "%s" @@ inductive_to_string i

let constructor_to_string (ind, x) =
  let _, ind = Global.lookup_inductive ind in
  let constr = ind.mind_consnames.(x - 1) in
  Id.to_string constr

let print_constructor fmt c =
  Format.fprintf fmt "%s" @@ constructor_to_string c

let projection_to_string p =
  Label.to_string (Projection.Repr.label p)

let print_projection fmt p =
  Format.fprintf fmt "%s" @@ projection_to_string p

let float64_to_float f =
  let open Float64 in
  if is_nan f then Float.nan
  else if is_infinity f then Float.infinity
  else if is_neg_infinity f then Float.neg_infinity
  else float_of_string @@ Float64.to_string f

type mutind = MutInd.t [@printer print_mutind] [@@deriving show]
type primitive = CPrimitives.t [@printer fun fmt p -> fprintf fmt "%s" (CPrimitives.to_string p)][@@deriving show]
type float64 = Float64.t [@printer fun fmt f -> fprintf fmt "%s" (Float64.to_string f)] [@@deriving show]
type uint63 = Uint63.t [@printer fun fmt n -> fprintf fmt "%s" (Uint63.to_string n)] [@@deriving show]
type projection = Projection.Repr.t [@printer print_projection] [@@deriving show]
type name = Name.t [@printer fun fmt n -> fprintf fmt "%s" (Pp.string_of_ppcmds @@ Name.print n)][@@deriving show]
type inductive = Names.inductive [@printer print_inductive] [@@deriving show]
type constructor = Names.constructor [@printer print_constructor] [@@deriving show]
type constant = Constant.t
                [@printer fun fmt c -> fprintf fmt "%s" (Label.to_string @@ Constant.label c)][@@deriving show]
type id = Id.t [@printer fun fmt id -> fprintf fmt "%s" (Id.to_string id)] [@@deriving show]

type 'node proof_state =
  { ps_string     : string
  ; root          : 'node
  ; context       : (Id.t * 'node) list
  ; evar          : Evar.t }

type 'node outcome =
  { term               : 'node
  ; term_text          : string
  ; arguments          : 'node option list
  ; proof_state_before : 'node proof_state
  ; proof_states_after : 'node proof_state list }

type tactic =
  { tactic        : string
  ; base_tactic   : string
  ; interm_tactic : string
  ; tactic_hash   : int
  ; tactic_exact  : bool }

type 'node tactical_step =
  { tactic   : tactic option
  ; outcomes : 'node outcome list }

type 'node definition_type =
  | Ind of inductive (* TODO: Universes? *)
  | Construct of constructor (* TODO: Universes? *)
  | Proj of projection (* TODO: Resolve *)
  | ManualConst of constant (* TODO: Universes? *)
  | TacticalConstant of constant * 'node tactical_step list (* TODO: Universes? *)
  | ManualSectionConst of Id.t (* TODO: Universes? *)
  | TacticalSectionConstant of Id.t * 'node tactical_step list (* TODO: Universes? *)

type 'node def_status =
  | DOriginal
  | DDischarged of 'node
  | DSubstituted of 'node

type 'node definition' =
  { previous : 'node option
  ; external_previous : 'node list
  ; status : 'node def_status
  ; path : Libnames.full_path
  ; type_text : string
  ; term_text : string option
  ; def_type : 'node definition_type }

let print_definition { previous ; def_type; _ } =
  match def_type with
  | Ind c -> "Ind " ^ inductive_to_string c
  | Construct c -> "Construct " ^ constructor_to_string c
  | Proj p -> "Proj " ^ projection_to_string p
  | ManualConst c -> "Const " ^ Label.to_string @@ Constant.label c
  | TacticalConstant (c, _) -> "Const " ^ Label.to_string @@ Constant.label c
  | ManualSectionConst id -> "SecConst " ^ Id.to_string id
  | TacticalSectionConstant (id, _) -> "SecConst " ^ Id.to_string id

type 'node definition = 'node definition'
                        [@printer fun fmt c -> fprintf fmt "%s" (print_definition c)][@@deriving show]

type 'node node_type =
  | ProofState
  | UndefProofState

  (* Context *)
  | ContextDef of int * id
  | ContextAssum of int * id

  (* Definitions *)
  | Definition of 'node definition
  | ConstEmpty (* Helper to deal with definitions that don't have a body *)

  (* Sorts *)
  | SortSProp
  | SortProp
  | SortSet
  | SortType (* Collapsed universe *)

  (* Constr nodes *)
  | Rel
  | Evar of int
  | EvarSubst
  | Cast (* TODO: Do we want cast kind? *)
  | Prod of name
  | Lambda of name
  | LetIn of name
  | App
  | Case
  | CaseBranch
  | Fix (* TODO: Recursive var info? *)
  | FixFun of name
  | CoFix
  | CoFixFun of name

  | Int of uint63 (* TODO: Centralize *)
  | Float of float64 (* TODO: Centralize *)
  | Primitive of primitive (* TODO: Centralize *) [@@deriving show { with_path = false }]

type edge_type =
  (* Contexts *)
  | ContextElem
  | ContextSubject

  (* Context elements *)
  | ContextDefType
  | ContextDefTerm

  (* Constants *)
  | ConstType
  | ConstUndef
  | ConstDef
  | ConstOpaqueDef
  | ConstPrimitive

  (* Inductives *)
  | IndType
  | IndConstruct
  | ProjTerm
  | ConstructTerm

  (* Casts *)
  | CastTerm
  | CastType

  (* Products *)
  | ProdType
  | ProdTerm

  (* Lambdas *)
  | LambdaType
  | LambdaTerm

  (* LetIns *)
  | LetInDef
  | LetInType
  | LetInTerm

  (* Apps *)
  | AppFun
  | AppArg

  (* Cases *)
  | CaseTerm
  | CaseReturn
  | CaseBranchPointer
  | CaseInd

  (* CaseBranches *)
  | CBConstruct
  | CBTerm

  (* Fixes *)
  | FixMutual
  | FixReturn

  (* FixFuns *)
  | FixFunType
  | FixFunTerm

  (* CoFixes *)
  | CoFixMutual
  | CoFixReturn

  (* CoFixFuns *)
  | CoFixFunType
  | CoFixFunTerm

  (* Backpointers *)
  | RelPointer

  (* Evars *)
  | EvarSubstPointer
  | EvarSubstTerm
  | EvarSubstTarget
  | EvarSubject
[@@deriving show { with_path = false }]

let edge_type_int_mod = function
  | ContextElem -> 0
  | ContextSubject -> 1
  | ContextDefType -> 0
  | ContextDefTerm -> 1
  | ConstType -> 0
  | ConstUndef -> 1
  | ConstDef -> 2
  | ConstOpaqueDef -> 3
  | ConstPrimitive -> 4
  | IndType -> 0
  | IndConstruct -> 1
  | ProjTerm -> 0
  | ConstructTerm -> 0
  | CastType -> 0
  | CastTerm -> 1
  | ProdType -> 0
  | ProdTerm -> 1
  | LambdaType -> 0
  | LambdaTerm -> 1
  | LetInDef -> 0
  | LetInType -> 1
  | LetInTerm -> 2
  | AppFun -> 0
  | AppArg -> 1
  | CaseTerm -> 0
  | CaseReturn -> 1
  | CaseBranchPointer -> 2
  | CaseInd -> 3
  | CBConstruct -> 0
  | CBTerm -> 1
  | FixMutual -> 0
  | FixReturn -> 1
  | FixFunType -> 0
  | FixFunTerm -> 1
  | CoFixMutual -> 0
  | CoFixReturn -> 1
  | CoFixFunType -> 0
  | CoFixFunTerm -> 1
  | RelPointer -> 0
  | EvarSubstPointer -> 0
  | EvarSubstTerm -> 0
  | EvarSubstTarget -> 1
  | EvarSubject -> 1

module type GraphMonadType = sig
  include Monad.Def
  type node
  type node_label
  type edge_label
  type children = (edge_label * node) list
  type 'a repr_t
  val mk_node : node_label -> children -> node t
  val with_delayed_node : ?definition:bool -> (node -> ('a * node_label * children) t) -> 'a t
  val register_external : node -> unit t
  val run : 'a t -> 'a repr_t
end

module DList : sig
  type 'a t
  val nil : 'a t
  val append : 'a t -> 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val singleton : 'a -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end = struct
  type 'a t = 'a list -> 'a list
  let nil = fun tl -> tl
  let append ls1 ls2 = fun tl -> ls1 (ls2 tl)
  let cons x ls = fun tl -> x :: ls tl
  let singleton x = fun tl -> x::tl
  let of_list ls = fun tl -> ls @ tl
  let to_list ls = ls []
end

module SimpleGraph
    (D : sig
       type node_label
       type edge_label
       type result
     end)
  : GraphMonadType
  with type node_label = D.node_label
   and type edge_label = D.edge_label
   and type node = int
   and type 'a repr_t =
         'a *
         ((node_count:int -> edge_count:int -> D.result) ->
          (D.result -> D.node_label -> (D.edge_label * int) list -> D.result) ->
          D.result)
= struct
  include D
  type node = int
  type children = (edge_label * node) list
  type writer = int * ((result -> node_label -> (edge_label * int) list -> result) -> result -> result)
  module M = Monad_util.StateWriterMonad
      (struct type s = node end)
      (struct type w = writer
        let id = 0, fun _ r -> r
        let comb (ec1, f1) (ec2, f2) = ec1 + ec2, fun c r -> f1 c (f2 c r) end)
  include M
  type 'a repr_t =
    'a *
    ((node_count:int -> edge_count:int -> D.result) ->
     (result -> node_label -> (edge_label * int) list -> result) ->
     result)
  open Monad_util.WithMonadNotations(M)
  let mk_node nl ch =
    let* i = get in
    put (i + 1) >>
    let+ () = tell (0, fun c r -> c r nl ch) in
    i
  let with_delayed_node ?definition:_ f =
    let* i = get in
    put (i + 1) >>
    pass @@
    let+ (v, nl, ch) = f i in
    v, fun (ec, nls) -> (ec + List.length ch), fun c r -> c (nls c r) nl ch
  let register_external _ = return ()
  let run m : 'a repr_t =
    let node_count, ((edge_count, ns), res) = run m 0 in
    res, fun mki c -> let r = mki ~node_count ~edge_count in ns c r
end

module AList : sig
  type 'a t
  val nil : 'a t
  val append : 'a t -> 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val rcons : 'a t -> 'a -> 'a t
  val singleton : 'a -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val fold : ('b -> 'a -> 'b) -> 'a t -> 'b -> 'b
end = struct
  type 'a t =
    | Nil
    | Singleton of 'a
    | Append of 'a t * 'a t
  let nil = Nil
  let append ls1 ls2 = match ls1, ls2 with
    | Nil, _ -> ls2
    | _, Nil -> ls1
    | _, _ -> Append (ls1, ls2)
  let cons x ls = match ls with
    | Nil -> Singleton x
    | _ -> Append (Singleton x, ls)
  let rcons ls x = match ls with
    | Nil -> Singleton x
    | _ -> Append (ls, Singleton x)
  let singleton x = Singleton x
  let of_list ls =
    match ls with
    | [] -> Nil
    | x::ls -> List.fold_left (fun ls' x -> rcons ls' x) (Singleton x) ls
  let to_list ls =
    let rec aux acc ls =
      match ls with
      | Nil -> acc
      | Singleton x -> x::acc
      | Append (ls1, ls2) -> aux (aux acc ls2) ls1 in
    aux [] ls
  let fold f ls init =
    let rec aux acc ls =
      match ls with
      | Nil -> acc
      | Singleton x -> f acc x
      | Append (ls1, ls2) -> aux (aux acc ls1) ls2 in
    aux init ls
end

type edge_label = edge_type
type edge_range = { start : int; size : int }
type ('p, 'nl, 'el, 'n) builder =
  { paths : 'p
  ; def_count : int
  ; node_count : int
  ; edge_count : int
  ; defs : ('nl * edge_range) AList.t
  ; nodes : ('nl * edge_range) AList.t
  ; edges : ('el * 'n) AList.t }
(* TODO: See if this can be merged with SimpleGraph *)
module GlobalGraph(S : Set.S)
    (D : sig
       type node_label
       type edge_label
       val is_definition : node_label -> bool
     end)
  : sig
  type node' = S.elt * (bool * int)
  type nonrec builder = (S.t, D.node_label, D.edge_label, node') builder
  val builder_nil : builder
  include GraphMonadType
  with type node_label = D.node_label
   and type edge_label = D.edge_label
   and type node = node'
   and type 'a repr_t = builder -> S.elt -> 'a * builder
end = struct
  type node' = S.elt * (bool * int)
  type node = node'
  type nonrec builder = (S.t, D.node_label, D.edge_label, node') builder
  let builder_nil = { paths = S.empty; def_count = 0; node_count = 0; edge_count = 0
                    ; defs = AList.nil; nodes = AList.nil; edges = AList.nil }
  type edge_label = D.edge_label
  type node_label = D.node_label
  type children = (edge_label * node) list
  type state =
    { def_count : int
    ; node_count : int
    ; edge_count : int }
  type writer =
    { defs : (node_label * edge_range) AList.t
    ; nodes : (node_label * edge_range) AList.t
    ; edges : (edge_label * node') AList.t
    ; paths : S.t }
  module M = Monad_util.ReaderStateWriterMonad
      (struct type r = S.elt end)
      (struct type s = state end)
      (struct type w = writer
        let id = { defs = AList.nil; nodes = AList.nil; edges = AList.nil; paths = S.empty }
        let comb = fun
          { defs = d1; nodes = f1; edges = e1; paths = p1 }
          { defs = d2; nodes = f2; edges = e2; paths = p2 } ->
          { defs = AList.append d1 d2
          ; nodes = AList.append f1 f2
          ; edges = AList.append e1 e2
          ; paths = S.union p1 p2 }
      end)
  include M
  type nonrec 'a repr_t = builder -> S.elt -> 'a * builder
  open Monad_util.WithMonadNotations(M)
  let index_to_node i =
    let+ current = ask in
    current, i
  let children_paths ch ps =
    List.fold_left (fun ps (_, (p, _)) -> S.add p ps) ps ch
  let mk_node nl ch =
    let* { def_count; node_count; edge_count } = get in
    let edge_count' = List.length ch in
    let* defs, nodes, i =
      match D.is_definition nl with
      | true ->
        let+ () = put { def_count = def_count + 1; node_count; edge_count = edge_count + edge_count' } in
        AList.singleton (nl, { start = edge_count; size = edge_count' }), AList.nil, (true, def_count)
      | false ->
        let+ () = put { def_count; node_count = node_count + 1; edge_count = edge_count + edge_count' } in
        AList.nil, AList.singleton (nl, { start = edge_count; size = edge_count' }), (false, node_count)
    in
    let* () = tell { defs; nodes; edges = AList.of_list ch; paths = children_paths ch S.empty } in
    index_to_node i
  let with_delayed_node ?(definition=false) f =
    let* { def_count; node_count; edge_count } = get in
    let* i =
      match definition with
      | true ->
        let+ () = put { def_count = def_count + 1; node_count; edge_count } in
        true, def_count
      | false ->
        let+ () = put { def_count; node_count = node_count + 1; edge_count } in
        false, node_count
    in
    pass @@
    let* n = index_to_node i in
    let* v, nl, ch = f n in
    let* { edge_count; _ } as s = get in
    let edge_count' = List.length ch in
    let+ () = put {s with edge_count = edge_count + edge_count' } in
    v, fun { defs; nodes; edges; paths } ->
      let defs, nodes =
        match definition, D.is_definition nl with
        | true, true ->
          AList.cons (nl, { start = edge_count; size = edge_count' }) defs, nodes
        | false, false ->
          defs, AList.cons (nl, { start = edge_count; size = edge_count' }) nodes
        | _, _ -> assert false in
      { defs; nodes; edges = AList.append edges (AList.of_list ch)
      ; paths = children_paths ch paths }
  let register_external (tp, _) =
    tell { defs = AList.nil; nodes = AList.nil; edges = AList.nil; paths = S.singleton tp }
  let run m { paths; def_count; node_count; edge_count; defs; nodes; edges } current =
    let { def_count; node_count; edge_count }, ({ defs; nodes; edges; paths }, result) =
      let m = tell { defs; nodes; edges; paths } >> m in
      run m current { def_count; node_count; edge_count } in
    result, { paths; def_count; node_count; edge_count
            ; defs; nodes; edges }
end

module WeakHasher = struct
  type t = int
  type state = int
  let with_state f = f 0
  let update = Hashset.Combine.combine
  let update_int = Hashset.Combine.combine
  let update_string s = Hashset.Combine.combine (Hashtbl.hash s)
  let compare = Int.compare
  let to_int x = x
end

module XXHasher = struct
  open XXHash
  let int_buffer = Bytes.create 8
  type t = int64
  type state = XXH64.state
  let with_state f = XXH64.with_state (fun s -> ignore (f s))
  let update i state =
    Bytes.set_int64_be int_buffer 0 i;
    XXH64.update state @@ Bytes.unsafe_to_string int_buffer;
    state
  let update_int i state =
    Bytes.set_int64_ne int_buffer 0 @@ Int64.of_int i;
    XXH64.update state @@ Bytes.unsafe_to_string int_buffer;
    state
  let update_string s state = XXH64.update state s; state
  let compare = Int64.compare
  let to_int = Int64.to_int
end

module CICHasher
    (H : sig
       type t
       type state
       val with_state      : (state -> state) -> t
       val update          : t -> state -> state
       val update_int      : int -> state -> state
       val update_string   : string -> state -> state
       val compare         : t -> t -> int
       val to_int          : t -> int (* Conversion to an integer is allowed to be lossy (cause collisions) *)
     end) = struct
  include H
  (* Definitions are never equal to any other definition, even if their names, type and body are equal.
     The reason is that they are also part of a global context. We don't want the hash of a definition
     to rely on a particular global context, but at the same time, we can also not merge two nodes with
     a different global context... *)
  let never_equal = function
    | Definition _ -> true
    | _ -> false
  let update_node_label p =
    let u = update_int in
    match p with
    | ProofState -> u 0
    | UndefProofState -> u 1
    | ContextDef (idx, _) -> fun s -> u 2 @@ update_int idx s
    | ContextAssum (idx, _) -> fun s -> u 3 @@ update_int idx s
    | Definition { path; _ } -> fun s -> u 4 @@ update_string (Libnames.string_of_path path) s
    | ConstEmpty -> u 5
    | SortSProp -> u 6
    | SortProp -> u 7
    | SortSet -> u 8
    | SortType -> u 9
    | Rel -> u 10
    | Evar e -> fun s -> u 11 @@ u e s (* Special care needs to be taken because two evars are never equal *)
    | EvarSubst -> u 12
    | Cast -> u 13
    | Prod _ -> u 14
    | Lambda _ -> u 15
    | LetIn _ -> u 16
    | App -> u 17
    | Case -> u 18
    | CaseBranch -> u 19
    | Fix -> u 20
    | FixFun _ -> u 21
    | CoFix -> u 22
    | CoFixFun _ -> u 23
    | Int i -> fun s -> u 24 @@ update_string (Uint63.to_string i) s (* Not very efficient but who cares *)
    | Float f -> fun s -> u 25 @@ update_string (Float64.to_string f) s (* Not very efficient but who cares *)
    | Primitive p -> fun s -> u 26 @@ update_string (CPrimitives.to_string p) s
  let update_edge_label =
    let u = update_int in
    function
    | ContextElem -> u 0
    | ContextSubject -> u 1
    | ContextDefType -> u 2
    | ContextDefTerm -> u 3
    | ConstType -> u 4
    | ConstUndef -> u 5
    | ConstDef -> u 6
    | ConstOpaqueDef -> u 7
    | ConstPrimitive -> u 8
    | IndType -> u 9
    | IndConstruct -> u 10
    | ProjTerm -> u 11
    | ConstructTerm -> u 12
    | CastTerm -> u 13
    | CastType -> u 14
    | ProdType -> u 15
    | ProdTerm -> u 16
    | LambdaType -> u 17
    | LambdaTerm -> u 18
    | LetInDef -> u 19
    | LetInType -> u 20
    | LetInTerm -> u 21
    | AppFun -> u 22
    | AppArg -> u 23
    | CaseTerm -> u 24
    | CaseReturn -> u 25
    | CaseBranchPointer -> u 26
    | CaseInd -> u 27
    | CBConstruct -> u 28
    | CBTerm -> u 29
    | FixMutual -> u 30
    | FixReturn -> u 31
    | FixFunType -> u 32
    | FixFunTerm -> u 33
    | CoFixMutual -> u 34
    | CoFixReturn -> u 35
    | CoFixFunType -> u 36
    | CoFixFunTerm -> u 37
    | RelPointer -> u 38
    | EvarSubstPointer -> u 39
    | EvarSubstTerm -> u 40
    | EvarSubstTarget -> u 41
    | EvarSubject -> u 42
end

(** `GraphHasher` is a module functor that transforms any `GraphMonadType` into another
    `GraphMonadType` such that bisimilar sub-graphs are shared. Additionally, every node
    is associated with a hash that identifies the node uniquely up to bisimulation. That
    is, two nodes have the same hash if and only if they are bisimilar. For the CIC graph
    extracted from Galina terms, it holds that the bisimulation relation is compatible
    with alpha-equivalence.

    This algorithm executes in O(n log n) time where n is the number of nodes. (It is
    assumed that the graph is sparse, and hence the number of edges is in the same order
    of magnitude as nodes).

    We make the following assumptions about the graph:

    1. Nodes that are bisimilar have forward closures of equal size. For general graphs
       this is not true. However, in lambda calculus, two terms are only alpha-equivalent
       when their AST is of equal size. Side-node: Special care must be taken during
       graph generation in order to ensure this. For example, with a naive graph-encoding
       the graphs of the class of terms
       (forall x, x) and (forall x y, y) and (forall x y z, z) are all bisimilar because
       they are unfoldings of each other.

    2. We assume that any node `y` generated in the context of
       `with_delayed_node (fun x -> ...)` is not bisimilar to `x`. We can interpret this
       restriction in terms of restrictions on graphs or restrictions on lambda terms:
       - Graphs: Any two nodes that are part of a cycle must not be equal.
       - Terms : Binders must be properly ordered. That is, set of mutually recursive
                 binders must not be mutually alpha-equivalent. Standard lambda calculus
                 adheres to this restriction. However, Gallina terms have two features
                 that almost violate this restriction:
                 - Mutually inductive definitions with the same structure. At first sight
                   these are binders that should be bisimilar and violate our restriction.
                   However, two inductive types or constructors can never be equal by
                   design (the names of the constructors are included in the identity of
                   their corresponding graph nodes). Hence, no violation occurs.
                 - Mutual fixpoints which have the same structure. This is more tricky
                   than inductives, because these structures are morally speaking actually
                   alpha-equivalent. However, in Coq, this equivalence is not really
                   observable. Note: In our current graph representation, these structures
                   are actually bisimilar. But one can imagine the name/ordering of the binders
                   in the fixpoint to be part of the identity of their corresponding graph
                   node, which fixes this problem.
*)
module GraphHasher
    (D : sig type node_label type edge_label end)
    (H : sig
       type t
       type state
       val with_state      : (state -> state) -> t
       val update          : t -> state -> state
       val update_int      : int -> state -> state
       (* val update_string   : string -> state -> state *)
       val update_node_label : D.node_label -> state -> state
       val update_edge_label : D.edge_label -> state -> state
       val compare         : t -> t -> int
       val to_int          : t -> int (* Conversion to an integer is allowed to be lossy (cause collisions) *)
       val never_equal     : D.node_label -> bool (* Nodes with these labels are never equal to any other node *)
     end)
    (G : GraphMonadType with type node_label = D.node_label * H.t and type edge_label = D.edge_label)
  : sig
    include GraphMonadType
      with type node_label = D.node_label
       and type edge_label = D.edge_label
       and type 'a repr_t = G.node Map.Make(H).t -> (G.node Map.Make(H).t * 'a) G.repr_t
    val lower : node -> G.node * H.t
    val lift : G.node * H.t -> node
  end
= struct
  type node_label = D.node_label
  type edge_label = D.edge_label
  type binder_info =
    { is_definition : bool option
    ; final : bool
    ; seen : bool }
  type node_info =
    { label : node_label
    ; children : (edge_label * node) list
    ; hash : H.t
    ; size : int
    ; id : int
    ; depth : int
    ; min_binder_referenced : int }
  and node' =
    | Normal of node_info
    | BinderPlaceholder of { depth : int }
    | Binder of node_info * binder_info
    | Written of G.node * H.t
  and node = node' ref
  let gen_id =
    let id = ref 0 in
    fun () ->
      id := !id + 1;
      !id
  let lower n = match !n with
    | Written (n, h) -> n, h
    | _ -> assert false
  let lift (g, h) = ref @@ Written (g, h)
  type children = (edge_label * node) list
  module HashMap = Map.Make(H)
  type 'a repr_t = G.node HashMap.t -> (G.node HashMap.t * 'a) G.repr_t

  module M = Monad_util.ReaderStateMonadT
      (G)
      (struct type r = int end) (* Current binder depth *)
      (struct type s = G.node HashMap.t end) (* TODO: Consider replacing this with a mutable hashmap for speed *)

  module OList = List
  open M
  open Monad.Make(M)
  include Monad_util.WithMonadNotations(M)

  let calc_hash curr_depth nl ch =
    (* Technically speaking, we should sort the hashes to make the final hash invariant w.r.t. child ordering.
       However, because the ordering is deterministic in practice, we don't need to do that. *)
    let hashes state = OList.fold_left (fun state (el, n) ->
        let n = match !n with
          | Written (_, hash) -> hash
          | BinderPlaceholder _ -> H.with_state @@ fun x -> x
          | Normal { hash; _ } -> hash
          | Binder ({ hash; _ }, { seen = false; _ }) -> hash
          | Binder ({ depth; label; _ }, { seen = true; final = false; _ }) ->
              H.with_state @@ fun state ->
              (* Careful to include the label of the binder as well as its de Bruijn index *)
              H.update_node_label label @@ H.update_int (curr_depth - depth) state
          | Binder ({ hash; _ }, { seen = true; final = true; _ }) -> hash
        in
        H.update n @@ H.update_edge_label el state
      ) state ch in
    H.with_state @@ fun state ->
    H.update_node_label nl @@ hashes state

  let calc_min_binder_referenced ch =
    OList.fold_left (fun acc (_, n) -> match !n with
        | BinderPlaceholder { depth } -> min acc depth
        | Normal { min_binder_referenced; _ } | Binder ({ min_binder_referenced; _}, _) ->
          min acc min_binder_referenced
        | Written _ -> acc) max_int ch

  (** `node_size n` is not really the size of the forward closure, but rather a metric such that
      `node_size n != node_size m` implies that n and m are not bisimilar and for any subterm n'
      of n we have `node_size n > node_size n'`.
  *)
  let node_size n =
    match !n with
    | Written (_, hash) ->
      (* Any positive value such that written nodes that are equal have the same value works here.
         A constant 0 would be sufficient. But this would make the algorithm less efficient.
         So we use the absolute value of the hash. In order to prevent integer overflows, we reduce
         the hash to 32 bits. *)
      (abs @@ H.to_int hash) mod 4294967296
    | Normal { size; _ } -> size
    | Binder (_, { seen = true; _ }) -> 0
    | Binder ({ size; _ }, { seen = false; _ }) -> size
    | BinderPlaceholder _ -> 0
  let calc_size ch =
    let size = OList.map (fun (el, n) -> node_size n) ch in
    OList.fold_left (+) 1 size

  let rec recalc_hash n =
    match !n with
    | Written _ | Binder (_, { seen = true; _ }) -> ()
    | Binder (_, { seen = false; final = true; _ }) -> assert false
    | Binder (({ label; children; depth; _ } as i), ({ seen = false; _ } as bi)) ->
      n.contents <- Binder (i, { bi with seen = true });
      OList.iter recalc_hash @@ OList.map snd children;
      let hash = calc_hash depth label children in
      n.contents <- Binder ({ i with hash }, bi)
    | Normal ({ label; children; depth; _ } as i) ->
      OList.iter recalc_hash @@ OList.map snd children;
      let hash = calc_hash depth label children in
      n.contents <- Normal { i with hash }
    | BinderPlaceholder _ -> assert false

  let share_node nl hash conta contb =
    let add_node node =
      let* map = get in
      put (HashMap.add hash node map) in
    if H.never_equal nl then conta (fun _ -> return ()) else
      let* map = get in
      match HashMap.find_opt hash map with
      | None -> conta add_node
      | Some node -> contb node

  let write_node_and_children connected_component_hash n =
    let tag_connected_component hash = H.with_state @@ fun state ->
      H.update connected_component_hash @@ H.update hash state in
    let rec aux n =
      match !n with
      | Written (n, _) -> return n
      | Normal { label; children; hash; _ } ->
        let hash = tag_connected_component hash in
        let* ch = List.map (fun (el, n) -> let+ n = aux n in el, n) children in
        share_node label hash
          (fun add ->
             let* node = lift @@ G.mk_node (label, hash) ch in
             let+ () = add node in
             n.contents <- Written (node, hash);
             node)
          (fun node -> n.contents <- Written (node, hash); return node)
      | Binder ({ label; children; hash; _ }, { final = true; is_definition; _ }) ->
        let hash = tag_connected_component hash in
        share_node label hash
          (fun add ->
             let* depth = ask in
             let* map = get in
             let* map, node = lift @@ G.with_delayed_node ?definition:is_definition @@ fun node ->
               let computation =
                 let* () = add node in
                 n.contents <- Written (node, hash);
                 let+ ch = List.map (fun (el, n) -> let+ n = aux n in el, n) children in
                 node, (label, hash), ch in
               G.map (fun (a, (b, c, d)) -> (a, b), c, d) @@
               run computation depth (* depth really doesn't matter*) map in
             let+ () = put map in
             node)
          (fun node ->
             n.contents <- Written (node, hash);
             let+ _ = List.map (fun (el, n) -> let+ n = aux n in el, n) children in
             node)
      | Binder (_, { final = false; _ }) | BinderPlaceholder _ -> assert false in
    aux n

  module NodeSet = Set.Make(
    struct
      type t = node
      let compare n1 n2 =
        let cmp = Int.compare (node_size n1) (node_size n2) in
        if cmp != 0 then cmp else
          match !n1, !n2 with
          | (Normal { id = id1; _ } | Binder ({ id = id1; _ }, _)),
            (Normal { id = id2; _ } | Binder ({ id = id2; _ }, _)) -> Int.compare id1 id2
          | _ -> assert false
    end)

  (* Preconditions:
     - [node] is a binder that closes the subterm below it
     - Any subterm of [node] that is closed is already written to the underlying graph *)
  let converge_hashes node =
    let connected_component_hash = match !node with
      | Binder ({ hash; _}, _) -> hash
      | _ -> assert false in
    let add_filtered_children ch rem = OList.fold_left
        (fun rem (_, ch) -> match !ch with
           | Binder (_, { seen = true; _ }) | Written _ -> rem
           | _ -> NodeSet.add ch rem) rem ch in
    let decompose_node n rem =
      match n.contents with
      | Written (_, _) | Binder (_, { seen = true; _ }) | BinderPlaceholder _ -> assert false
      | Normal { children; _ } -> add_filtered_children children rem
      | Binder (({ label; children; _ } as i), ({ seen = false; _ } as bi)) ->
        n.contents <- Binder (i, { bi with final = true; seen = true });
        add_filtered_children children rem in
    let rec decompose_queue q =
      match NodeSet.max_elt_opt q with
      | None -> return ()
      | Some max ->
        let nz = node_size max in
        let minmax = NodeSet.find_first (fun n -> node_size n = nz) q in
        let rem, b, equal = NodeSet.split minmax q in
        assert b;
        (* Here is the crux of our algorithm: A naive algorithm, would always run `recalc_hash` on all
           nodes in order to make sure that every node receives a correct hash. This would lead to a
           O(n^2) runtime. To speed that up, we recognize that two nodes can only be equal if their size
           is equal. Hence, if only one node of a certain size exists, it does not need to be rehashed.
           This reduces our complexity to O(n log n). *)
        if not @@ NodeSet.is_empty equal then begin
          NodeSet.iter recalc_hash equal;
          recalc_hash minmax
        end;
        decompose_queue @@ NodeSet.fold decompose_node equal (decompose_node minmax rem)
    in
    let* () = decompose_queue @@ NodeSet.singleton node in
    write_node_and_children connected_component_hash node

  let children_converged =
    CList.fold_left (fun ch -> function
        | el, { contents = Written (n, _); _ } -> Option.map (fun ls -> (el, n)::ls) ch
        | _ -> None) (Some [])

  let mk_node : node_label -> children -> node t = fun nl ch ->
    let* depth = ask in
    let hash = calc_hash depth nl ch in
    match children_converged ch with
    | Some ch ->
      share_node nl hash
       (fun add ->
         let* node = lift @@ G.mk_node (nl, hash) ch in
         let+ () = add node in
         ref @@ Written (node, hash))
       (fun node -> return @@ ref @@ Written (node, hash))
    | None ->
      let size = calc_size ch in
      let min_binder_referenced = calc_min_binder_referenced ch in
      let node = ref @@ Normal
        { label = nl
        ; children = ch
        ; size
        ; hash
        ; id = gen_id ()
        ; depth
        ; min_binder_referenced } in
      return node

  let with_delayed_node : ?definition:bool -> (node -> ('a * node_label * children) t) -> 'a t =
    fun ?definition f ->
    local (fun d -> d + 1) @@
    let* depth = ask in
    let node = ref @@ BinderPlaceholder { depth } in
    let* v, nl, ch = f node in
    let hash = calc_hash depth nl ch in
    match children_converged ch with
    | Some ch ->
      share_node nl hash
       (fun add ->
         let* node' = lift @@ G.mk_node (nl, hash) ch in
         let+ () = add node' in
         node.contents <- Written (node', hash);
         v)
       (fun node' ->
          node.contents <- Written (node', hash);
          return v)
    | None ->
      let size = calc_size ch in
      let min_binder_referenced = calc_min_binder_referenced ch in
      node.contents <- Binder
          ({ label = nl
           ; hash
           ; children = ch
           ; size
           ; depth
           ; min_binder_referenced
           ; id = gen_id () },
           { is_definition = definition
           ; final = false
           ; seen = false });
      let+ () =
        (* We could use [dept = min_binder] here, except for mutual fixpoints *)
        if depth <= min_binder_referenced then
          map (fun (_ : G.node) -> ()) @@ converge_hashes node
        else return () in
      v

  let register_external : node -> unit t = fun n ->
    match !n with
    | Written (n, _) -> lift @@ G.register_external n
    | _ ->
      (* TODO: We know that nothing needs to be done here, because the node is not really external.
               But this is not really a nice API this way... *)
      return ()
  let run : 'a t -> G.node HashMap.t -> (G.node HashMap.t * 'a) G.repr_t = fun m hashed ->
    G.run @@ run m 0 hashed
end
