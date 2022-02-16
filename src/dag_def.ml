open Tactician_ltac1_record_plugin
open Names
open Declarations

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

type node_type =
  (* Context *)
  | WithContext
  | Context
  | ContextDef of id
  | ContextAssum of id

  (* Constants *)
  | ConstUndef of constant
  | ConstDef of constant
  | ConstOpaqueDef of constant

  (* Inducives *)
  | Ind of inductive
  | IndBinder
  | IndConstructs
  | Construct of constructor
  | Proj of projection

  (* Constr nodes *)
  | Evar
  | EvarSubsts
  | Cast (* TODO: Do we want cast kind? *)
  | Prod of name
  | ProdBinder
  | Lambda of name
  | LambdaBinder
  | LetIn of name
  | LetInBinder
  | App
  | AppArgs
  | Case
  | CaseBranches
  | CaseBranch
  | Fix (* TODO: Recursive var info? *)
  | FixAuxes
  | FixFun of name
  | FixFunBinder
  | CoFix
  | CoFixAuxes
  | CoFixFun of name
  | CoFixFunBinder

  (* Leafs*)
  | Int of uint63
  | Float of float64
  | ConstPrimitive of primitive (* TODO: Should we make this explicit? *)
  | SortSProp
  | SortProp
  | SortSet
  | SortType (* Collapsed universe *)
  | EvarId of int (* TODO: Should be resolved *) [@@deriving show { with_path = false }]

module type DAG = sig
  type t
  type node
  val empty : t
  val mk_node : t -> node_type -> node list -> (node * t)
end

module SimpleDAG = struct
  type node = int
  (* WARNING: This list needs to be reversed when interpreting node indexes *)
  type t = int * (node_type * node list) list
  let empty = 0, []
  let mk_node (l, g) nt children = (l, (l+1, (nt, children) :: g))
  let node_list (_, nodes) = List.rev nodes
end

module type DAG2 = sig
  include Monad.Def
  type node
  val mk_node : node_type -> node list -> node t
  val with_delayed_node : (node -> ('a * node_type * node list) t) -> 'a t
end

type 'a dlist = 'a list -> 'a list

let dlist_nil = fun tl -> tl
let dlist_append ls1 ls2 = fun tl -> ls1 (ls2 tl)
let dlist_cons x ls = fun tl -> x :: ls tl
let dlist_singleton x = fun tl -> x::tl

module SimpleDAG2 : DAG2 = struct
  type node = int
  module M = Monad_util.StateWriterMonad
      (struct type s = int end)
      (struct type w = (node_type * node list) dlist
        let id = dlist_nil
        let comb = dlist_append end)
  include M
  open Monad_util.WithMonadNotations(M)
  let mk_node (nt : node_type) (children : node list) : node t =
    let* i = get in
    put (i + 1) >>
    let+ () = tell (dlist_singleton (nt, children)) in
    i
  let with_delayed_node (f : node -> ('a * node_type * node list) t) : 'a t =
    let* i = get in
    put (i+1) >>
    pass @@
    let+ (v, nt, children) = f i in
    v, dlist_cons (nt, children)
end
