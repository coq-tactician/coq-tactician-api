open Constr
open Names
open Declarations
open Context

type t =
  | TConst of Constant.t
  | TMutInd of MutInd.t
  | TVar of Id.t

let constr_in_order_traverse acc f g h c =
  let rec aux acc c =
    match Constr.kind c with
    | Var id -> h id acc
    | Const (c, _) -> f c acc
    | Constr.Ind ((m, _), _) -> g m acc
    | Constr.Construct (((m, _), _), _) -> g m acc
    | Constr.Proj (p, _) -> g (Projection.mind p) acc
    | _ -> Constr.fold (fun acc c -> aux acc c) acc c in
  aux acc c

let constant_in_order_traverse env acc f g h c =
  let { const_body; const_type; _ } = Environ.lookup_constant c env in
  let acc = constr_in_order_traverse acc f g h const_type in
  match const_body with
  | Undef _ -> acc
  | Def c -> constr_in_order_traverse acc f g h @@ Mod_subst.force_constr c
  | OpaqueDef c ->
    let c, _ = Opaqueproof.force_proof Library.indirect_accessor (Environ.opaque_tables env) c in
    constr_in_order_traverse acc f g h c
  | Primitive _ -> acc

let mutinductive_in_order_traverse env acc f g h m =
  let ({ mind_params_ctxt; mind_packets; mind_record; _ } as mb) =
    Environ.lookup_mind m env in
  let inds = Array.to_list mind_packets in
  List.fold_left (fun acc ({ mind_user_lc; _ } as ib) ->
      let acc = List.fold_left (fun acc typ -> constr_in_order_traverse acc f g h typ) acc @@
        Array.to_list mind_user_lc in
      let univs = Declareops.inductive_polymorphic_context mb in
      let inst = Univ.make_abstract_instance univs in
      let env = Environ.push_context ~strict:false (Univ.AUContext.repr univs) env in
      let typ = Inductive.type_of_inductive env ((mb, ib), inst) in
      constr_in_order_traverse acc f g h typ
    ) acc inds

let variable_in_order_traverse env acc f g h id =
  match Environ.lookup_named id env with
  | Named.Declaration.LocalAssum (_, typ) ->
    constr_in_order_traverse acc f g h typ
  | Named.Declaration.LocalDef (_, term, typ) ->
    constr_in_order_traverse (constr_in_order_traverse acc f g h term) f g h typ

type accum =
  { vseen : Id.Set.t
  ; cseen : Cset.t
  ; mseen : Mindset.t
  ; order : t list }

let order env vs cs ms =
  let rec f c ({ cseen; _ } as acc) =
      if Cset.mem c cseen || not @@ Cset.mem c cs then acc else
        let { vseen; cseen; mseen; order } = constant_in_order_traverse env acc f g h c in
        { vseen; cseen = Cset.add c cseen; mseen; order = (TConst c) :: order }
  and g m ({ mseen; _ } as acc) =
      if Mindset.mem m mseen || not @@ Mindset.mem m ms then acc else
        let { vseen; cseen; mseen; order } = mutinductive_in_order_traverse env acc f g h m in
        { vseen; cseen; mseen = Mindset.add m mseen; order = (TMutInd m) :: order }
  and h v ({ vseen; _ } as acc) =
      if Id.Set.mem v vseen || not @@ Id.Set.mem v vs then acc else
        let { vseen; cseen; mseen; order } = variable_in_order_traverse env acc f g h v in
        { vseen = Id.Set.add v vseen; cseen; mseen; order = (TVar v) :: order } in
  let acc = { vseen = Id.Set.empty; cseen = Cset.empty; mseen = Mindset.empty; order = [] } in
  let acc = Id.Set.fold h vs acc in
  let acc = Cset.fold f cs acc in
  let { order; _ } = Mindset.fold g ms acc in
  List.rev order
