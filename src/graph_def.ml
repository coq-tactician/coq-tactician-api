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
  | Root

  (* Local Variable *)
  | LocalDef of id
  | LocalDefType
  | LocalDefTerm
  | LocalAssum of id

  (* Constants *)
  | Const of constant (* TODO: Universes? *)
  | ConstType
  | ConstUndef
  | ConstDef
  | ConstOpaqueDef
  | ConstPrimitive

  (* Inducives *)
  | Ind of inductive (* TODO: Universes? *)
  | Construct of constructor (* TODO: Universes? *)
  | Proj of projection (* TODO: Resolve *)

  (* Sorts *)
  | Sort
  | SProp
  | Prop
  | Set
  | Type (* Collapsed universe *)

  (* Constr nodes *)
  | Rel
  | Var
  | Evar of int (* TODO: This could be resolved *)
  | EvarSubst
  | Cast (* TODO: Do we want cast kind? *)
  | CastTerm
  | CastType
  | Prod of name
  | ProdType
  | ProdTerm
  | Lambda of name
  | LambdaType
  | LambdaTerm
  | LetIn of name
  | LetInDef
  | LetInType
  | LetInTerm
  | App
  | AppFun
  | AppArg
  | Case
  | CaseTerm
  | CaseReturn
  | CaseBranch
  | CBConstruct
  | CBTerm
  | Fix (* TODO: Recursive var info? *)
  | FixFun of name
  | FixFunType
  | FixFunTerm
  | FixReturn
  | CoFix
  | CoFixFun of name
  | CoFixFunType
  | CoFixFunTerm
  | CoFixReturn

  | Int of uint63 (* TODO: Centralize *)
  | Float of float64 (* TODO: Centralize *)
  | Primitive of primitive (* TODO: Centralize *) [@@deriving show { with_path = false }]

module type Graph = sig
  type t
  type node
  val empty : t
  val mk_node : t -> node_type -> node * t
  val mk_edge : t -> from:node -> toward:node -> t
end

module SimpleGraph = struct
  type node = int
  type directed_edge =
    { from : node
    ; toward : node }
  type t =
    { nodes : node_type list (* WARNING: This list needs to be reversed when interpreting node indexes *)
    ; last  : node
    ; assoc : directed_edge list }
  let empty = { nodes = []; last = 0; assoc = [] }
  let mk_node ({ nodes; last; _ } as g) typ =
    last, { g with nodes = typ::nodes; last = last + 1}
  let mk_edge ({ assoc; _ } as g) ~from ~toward =
    { g with assoc = { from; toward } :: assoc }
  let node_list { nodes; _ } = List.rev nodes
  let edge_list { assoc; _ } = assoc (* No need to reverse this *)
end
