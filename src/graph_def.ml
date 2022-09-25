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
  { ps_string : string
  ; root      : 'node
  ; context   : 'node list
  ; evar      : Evar.t }

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
  | ContextDef of id
  | ContextAssum of id

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
  | AppFun
  | AppArg
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
  | AppFunPointer
  | AppFunValue
  | AppArgPointer
  | AppArgValue
  | AppArgOrder

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
  | AppFunPointer -> 0
  | AppFunValue -> 0
  | AppArgPointer -> 1
  | AppArgValue -> 0
  | AppArgOrder -> 2
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

(* TODO: See if this can be merged with SimpleGraph *)
type edge_label = edge_type
type edge_range = { start : int; size : int }
module GlobalGraph(S : Set.S) : sig
  type node' = S.elt * (bool * int)
  type builder =
    { paths : S.t
    ; def_count : int
    ; node_count : int
    ; edge_count : int
    ; defs : (node' node_type * edge_range) AList.t
    ; nodes : (node' node_type * edge_range) AList.t
    ; edges : (edge_label * node') AList.t }
  val builder_nil : builder
  include GraphMonadType
  with type node_label = node' node_type
   and type edge_label = edge_label
   and type node = node'
   and type 'a repr_t = builder -> S.elt -> 'a * builder
end = struct
  type node' = S.elt * (bool * int)
  type node = node'
  type builder =
    { paths : S.t
    ; def_count : int
    ; node_count : int
    ; edge_count : int
    ; defs : (node' node_type * edge_range) AList.t
    ; nodes : (node' node_type * edge_range) AList.t
    ; edges : (edge_label * node') AList.t }
  let builder_nil = { paths = S.empty; def_count = 0; node_count = 0; edge_count = 0
                    ; defs = AList.nil; nodes = AList.nil; edges = AList.nil }
  type edge_label = edge_type
  type node_label = node node_type
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
      match nl with
      | Definition _ ->
        let+ () = put { def_count = def_count + 1; node_count; edge_count = edge_count + edge_count' } in
        AList.singleton (nl, { start = edge_count; size = edge_count' }), AList.nil, (true, def_count)
      | _ ->
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
        match definition, nl with
        | true, Definition _ ->
          AList.cons (nl, { start = edge_count; size = edge_count' }) defs, nodes
        | false, _ ->
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
      let k = Buffer.contents
  let update_node_label p =
    let u = update_int in
    match p with
    | ProofState -> u 0
    | UndefProofState -> u 1
    | ContextDef id -> fun s -> u 2 @@ update_string (Id.to_string id) s
    | ContextAssum id -> fun s -> u 3 @@ update_string (Id.to_string id) s
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
    | AppFun -> u 18
    | AppArg -> u 19
    | Case -> u 20
    | CaseBranch -> u 21
    | Fix -> u 22
    | FixFun _ -> u 23
    | CoFix -> u 24
    | CoFixFun _ -> u 25
    | Int i -> fun s -> u 26 @@ update_string (Uint63.to_string i) s (* Not very efficient but who cares *)
    | Float f -> fun s -> u 27 @@ update_string (Float64.to_string f) s (* Not very efficient but who cares *)
    | Primitive p -> fun s -> u 28 @@ update_string (CPrimitives.to_string p) s
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
    | AppFunPointer -> u 22
    | AppFunValue -> u 23
    | AppArgPointer -> u 24
    | AppArgValue -> u 25
    | AppArgOrder -> u 26
    | CaseTerm -> u 27
    | CaseReturn -> u 28
    | CaseBranchPointer -> u 29
    | CaseInd -> u 30
    | CBConstruct -> u 31
    | CBTerm -> u 32
    | FixMutual -> u 33
    | FixReturn -> u 34
    | FixFunType -> u 35
    | FixFunTerm -> u 36
    | CoFixMutual -> u 37
    | CoFixReturn -> u 38
    | CoFixFunType -> u 39
    | CoFixFunTerm -> u 40
    | RelPointer -> u 41
    | EvarSubstPointer -> u 42
    | EvarSubstTerm -> u 43
    | EvarSubstTarget -> u 44
    | EvarSubject -> u 45
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
     end)
    (G : GraphMonadType with type node_label = D.node_label * H.t and type edge_label = D.edge_label)
  : sig
    include GraphMonadType
      with type node_label = D.node_label
       and type edge_label = D.edge_label
       and type 'a repr_t = 'a G.repr_t
    val reduce : node -> G.node
  end
= struct
  type node_label = D.node_label
  type edge_label = D.edge_label
  type node' =
    | Written of G.node * H.t
    | Normal of
        { label : node_label
        ; hash : H.t
        ; children : (edge_label * node) list
        ; size : int }
    | BinderPlaceholder
    | Binder of
        { label : node_label
        ; hash : H.t
        ; children : (edge_label * node) list
        ; is_definition : bool option
        ; size : int
        ; final : bool
        ; seen : bool
        }
  and node = { id : int
             ; depth : int
             ; mutable contents : node' }
  let gen_node =
    let id = ref 0 in
    fun depth contents ->
      id := !id + 1;
      { id = !id; depth; contents }
  let reduce { contents; _ } = match contents with
    | Written (n, _) -> n
    | _ -> assert false
  type children = (edge_label * node) list
  type 'a repr_t = 'a G.repr_t

  module HashMap = Map.Make(H)

  module M = Monad_util.ReaderStateMonadT
      (G)
      (struct type r = int end)
      (struct type s = G.node HashMap.t end)

  module OList = List
  open M
  open Monad.Make(M)
  include Monad_util.WithMonadNotations(M)

  let calc_hash curr_depth nl ch =
    (* Technically speaking, we should sort the hashes to make the final hash invariant w.r.t. child ordering.
       However, because the ordering is deterministic in practice, we don't need to do that. *)
    let hashes state = OList.fold_left (fun state (el, { depth; contents; _ }) ->
        let n = match contents with
          | Written (_, hash) -> hash
          | Normal { hash; _ } -> hash
          | Binder { seen = false; hash; _ } -> hash
          | Binder { seen = true; final = false; _ } ->
            Feedback.msg_notice Pp.(str "binder at depth " ++ int depth ++ str " current depth " ++ int curr_depth ++ str " de Bruijn: " ++ int (curr_depth - depth));
            H.with_state @@ H.update_int (curr_depth - depth)
          | Binder { seen = true; final = true; hash; _ } -> hash
          | BinderPlaceholder -> H.with_state @@ H.update_int (curr_depth - depth)
        in
        H.update n @@ H.update_edge_label el state
      ) state ch in
    H.with_state @@ fun state ->
    H.update_node_label nl @@ hashes state

  (** `node_size n` is not really the size of the forward closure, but rather a metric such that
      `node_size n != node_size m` implies that n and m are not bisimilar and for any subterm n'
      of n we have `node_size n > node_size n'`.
      The constant function satisfies those requirements but would make this algorithm O(n^2).
  *)
  let node_size { contents; _ } =
    match contents with
    | Written (_, hash) ->
      (* Any positive value such that written nodes that are equal have the same value works here.
         A constant 0 would be sufficient. But this would make the algorithm less efficient.
         So we use the absolute value of the hash. In order to prevent integer overflows, we reduce
         the hash to 32 bits. *)
      (abs @@ H.to_int hash) mod 4294967296
    | Normal { size; _ } -> size
    | Binder { seen = true; _ } -> 0
    | Binder { seen = false; size; _ } -> size
    | BinderPlaceholder -> 0
  let calc_size ch =
    let size = OList.map (fun (el, n) -> node_size n) ch in
    let rews = OList.fold_left (+) 1 size in
    Feedback.msg_notice Pp.(str "calculating size " ++ int rews);
    rews

  let mk_node : node_label -> children -> node t = fun nl ch ->
    let* depth = ask in
    let hash = calc_hash depth nl ch in
    let converged = CList.for_all (function (_, { contents = Written _; _ }) -> true | _ -> false) ch in
    if converged then begin
      let* map = get in
      match HashMap.find_opt hash map with
      | Some node -> return @@ gen_node depth @@ Written (node, hash)
      | None ->
        let ch = OList.map (function | (el, { contents = Written (n, _); _ }) -> el, n | _ -> assert false) ch in
        let* node = lift @@ G.mk_node (nl, hash) ch in
        let+ () = put (HashMap.add hash node map) in
        Feedback.msg_notice Pp.(str "Add node with hash: " ++ int (H.to_int hash));
        gen_node depth @@ Written (node, hash)
    end else begin
      let size = calc_size ch in
      return @@ gen_node depth @@ Normal
        { label = nl
        ; children = ch
        ; size
        ; hash }
  end

  let rec recalc_hash ({ depth; contents; _ } as n) =
    Feedback.msg_notice Pp.(str "recalc hash");
    match contents with
    | Written _ ->
      Feedback.msg_notice Pp.(str "recalc hash : written")
    | Binder { seen = true; _ } ->
      Feedback.msg_notice Pp.(str "recalc hash : binder seen")
    | Binder { seen = false; final = true; _ } -> assert false
    | Binder ({ seen = false; label; children; _ } as n') ->
      Feedback.msg_notice Pp.(str "recalc hash : binder unseen");
      n.contents <- Binder { n' with seen = true };
      OList.iter recalc_hash @@ OList.map snd children;
      let hash = calc_hash depth label children in
      n.contents <- Binder { n' with hash }
    | Normal ({ label; children; _ } as n') ->
      Feedback.msg_notice Pp.(str "recalc hash : normal");
      OList.iter recalc_hash @@ OList.map snd children;
      let hash = calc_hash depth label children in
      n.contents <- Normal { n' with hash }
    | _ -> assert false

  let rec write_node_and_children ({ contents; _ } as n) =
    Feedback.msg_notice Pp.(str "write node and children");
    match contents with
    | Written (n, _) ->
      Feedback.msg_notice Pp.(str "write node and children : written");
      return n
    | Normal { hash; label; children; _ } ->
      Feedback.msg_notice Pp.(str "write node and children : normal");
      let* ch = List.map (fun (el, n) -> let+ n = write_node_and_children n in el, n) children in
      let* map = get in
      (match HashMap.find_opt hash map with
       | Some node -> n.contents <- Written (node, hash); return node
       | None ->
         let* node = lift @@ G.mk_node (label, hash) ch in
         let+ () = put (HashMap.add hash node map) in
         n.contents <- Written (node, hash);
         node)
    | Binder { hash; final = true; label; children; is_definition; _ } ->
      Feedback.msg_notice Pp.(str "write node and children : binder");
      let* map = get in
      (match HashMap.find_opt hash map with
       | Some node ->
         n.contents <- Written (node, hash);
         let+ _ = List.map (fun (el, n) -> let+ n = write_node_and_children n in el, n) children in
         node
       | None ->
         let* depth = ask in
         let* map, node = lift @@ G.with_delayed_node ?definition:is_definition @@ fun node ->
           let computation =
             let* () = put (HashMap.add hash node map) in
             n.contents <- Written (node, hash);
             let+ ch = List.map (fun (el, n) -> let+ n = write_node_and_children n in el, n) children in
             node, (label, hash), ch in
           G.map (fun (a, (b, c, d)) -> (a, b), c, d) @@ run computation depth (* depth really doesn't matter*) map in
         let+ () = put map in
         node
      )
    | _ -> assert false

  module NodeSet = Set.Make(
    struct
      type t = node
      let compare n1 n2 =
        let cmp = Int.compare (node_size n1) (node_size n2) in
        if cmp = 0 then Int.compare n1.id n2.id else cmp
    end)
  let with_delayed_node : ?definition:bool -> (node -> ('a * node_label * children) t) -> 'a t =
    fun ?definition f ->
    local (fun d -> d + 1) @@
    let* depth = ask in
    let node = gen_node depth @@ BinderPlaceholder in
    let* v, nl, ch = f node in
    let converged = CList.for_all (function (_, { contents = Written _; _ }) -> true | _ -> false) ch in
    let hash = calc_hash depth nl ch in
    if converged then begin
      let* map = get in
      match HashMap.find_opt hash map with
      | Some node' ->
        node.contents <- Written (node', hash);
        return v
      | None ->
        let ch = OList.map (function | (x, { contents = Written (n, _); _ }) -> x, n | _ -> assert false) ch in
        let* node' = lift @@ G.mk_node (nl, hash) ch in
        let+ () = put (HashMap.add hash node' map) in
        node.contents <- Written (node', hash);
        v
    end else begin
      let size = calc_size ch in
      Feedback.msg_notice @@ Pp.(str "binder created at depth " ++ int depth);
      node.contents <- Binder
          { label = nl
          ; hash
          ; children = ch
          ; is_definition = definition
          ; size
          ; final = false
          ; seen = false };
      if depth = 1 then begin
        Feedback.msg_notice Pp.(str "starting binder resolution");
        let add_filtered_children ch rem = OList.fold_left
            (fun rem (_, ch) -> match ch.contents with
               | Binder { seen = true; _ } | Written _ -> rem
               | _ -> NodeSet.add ch rem) rem ch in
        let decompose_node n rem =
          Feedback.msg_notice Pp.(str "decompose node");
          match n.contents with
          | Written (_, _) ->
            Feedback.msg_notice Pp.(str "decompose node written");
            rem
          | Binder { seen = true; _ } -> assert false
          | Normal { children; _ } ->
            Feedback.msg_notice Pp.(str "decompose node normal");
            add_filtered_children children rem
          | Binder ({ label; children; seen = false; _ } as d) ->
            Feedback.msg_notice Pp.(str "decompose node binder");
            n.contents <- Binder { d with final = true; seen = true };
            add_filtered_children children rem
          | BinderPlaceholder -> assert false in
        let rec decompose_queue q =
          Feedback.msg_notice Pp.(str "decompose queue");
          NodeSet.iter (fun n ->
              Feedback.msg_notice Pp.(str "node size in list: " ++ int (node_size n) ++ str " id: " ++ int n.id)) q;
          match NodeSet.max_elt_opt q with
          | None -> return ()
          | Some max ->
            let nz = node_size max in
            let minmax = NodeSet.find_first (fun n -> node_size n = nz) q in
            Feedback.msg_notice Pp.(str "node size: " ++ int nz);
            let rem, b, equal = NodeSet.split minmax q in
            assert b;
            Feedback.msg_notice Pp.(str "equal list size: " ++ int (NodeSet.cardinal equal));
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
        Feedback.msg_notice Pp.(str "pre write node");
        let+ _ = write_node_and_children node in
        v
      end else begin
        return v
      end
    end
  let register_external : node -> unit t = fun n ->
    match n.contents with
    | Written (n, _) -> lift @@ G.register_external n
    | _ ->
      (* TODO: We know that nothing needs to be done here, because the node is not really external.
                    But this is not really a nice API this way... *)
      return ()
  let run : 'a t -> 'a G.repr_t = fun m ->
    G.run @@ G.map snd @@ run m 0 HashMap.empty
end
