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

type 'node tactical_step =
  { ps_string: string
  ; tactic : Ltac_plugin.Tacexpr.glob_tactic_expr
  ; base_tactic : Ltac_plugin.Tacexpr.glob_tactic_expr
  ; interm_tactic : Ltac_plugin.Tacexpr.glob_tactic_expr
  ; tactic_hash : int
  ; arguments : 'node option list
  ; root : 'node
  ; context : 'node list }

type 'node definition_type =
  | Ind of inductive (* TODO: Universes? *)
  | Construct of constructor (* TODO: Universes? *)
  | Proj of projection (* TODO: Resolve *)
  | ManualConst of constant (* TODO: Universes? *)
  | TacticalConstant of constant * 'node tactical_step list (* TODO: Universes? *)

type 'node definition' =
  { previous : 'node list
  ; def_type : 'node definition_type }

let print_definition { previous ; def_type } =
  match def_type with
  | Ind c -> "Ind " ^ inductive_to_string c
  | Construct c -> "Construct " ^ constructor_to_string c
  | Proj p -> "Proj " ^ projection_to_string p
  | ManualConst c -> "Const " ^ Label.to_string @@ Constant.label c
  | TacticalConstant (c, _) -> "Const " ^ Label.to_string @@ Constant.label c

type 'node definition = 'node definition'
                        [@printer fun fmt c -> fprintf fmt "%s" (print_definition c)][@@deriving show]

type 'node node_type =
  | Root

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
  | Var
  | Evar of int (* TODO: This could be resolved *)
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

  (* Constr edges *)
  | RelPointer
  | VarPointer
  | EvarSubstPointer
  | EvarSubstOrder
  | EvarSubstValue
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
  | VarPointer -> 0
  | EvarSubstPointer -> 0
  | EvarSubstOrder -> 0
  | EvarSubstValue -> 1

module type GraphMonadType = sig
  include Monad.Def
  type node
  type node_label
  type edge_label
  type children = (edge_label * node) list
  type 'a repr_t
  val mk_node : node_label -> children -> node t
  val with_delayed_node : (node -> ('a * node_label * children) t) -> 'a t
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

type ('s, 't, 'edge_label) directed_edge =
  { source : 's
  ; target : 't
  ; label  : 'edge_label }
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
  let with_delayed_node f =
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
