open Tactician_ltac1_record_plugin
open Map_all_the_things
open Genarg
open Names
open Tactician_util
open Glob_term

type var_type =
  | TVar of Id.t
  | TRef of GlobRef.t
  | TOther

module FreeVarsDef = struct
  module M = ReaderWriterMonad
      (struct type r = Id.t list type w = var_type list let id = [] let comb = List.append end)
  include MapDefTemplate (M)
  let map_sort = "one_free_variable"
  let warnProblem wit =
    Feedback.msg_warning (Pp.(str "Tactician is having problems with " ++
                              str "the following tactic. Please report. " ++
                              pr_argument_type wit))
  let default wit = { raw = (fun _ -> warnProblem (ArgumentType wit); id)
                    ; glb = (fun _ -> warnProblem (ArgumentType wit); id)}

  let with_binders ids = M.local (fun ids' -> (ids@ids'))
end
module FreeVarsMapper = MakeMapper(FreeVarsDef)
open FreeVarsDef

let find_variable ls =
  let cs = List.filter (function
      | TVar _ | TOther -> false
      | TRef _ -> true) ls in
  let vs = List.filter (function
      | TRef _ | TOther -> false
      | TVar _ -> true) ls in
  if not (cs = []) then List.hd cs else
  if not (vs = []) then List.hd vs else
    TOther

let curtail c =
    M.censor (fun ls -> [find_variable ls]) @@ c

let mapper = { FreeVarsDef.default_mapper with
               variable = (fun id ->
                   M.(ask >>= fun ids ->
                      (if List.exists (Id.equal id) ids then return () else tell [TVar id]) >> return id))
             ; constant = (fun c ->
                   M.(tell [TRef (GlobRef.ConstRef c)] >> return c))
             ; glob_constr_and_expr = (fun c cont ->
                   M.censor (function
                       | [] -> [TOther]
                       | x -> x) @@
                   let+ c, _ = cont c in
                   c, None
                 )
             ; glob_constr = (fun c cont ->
                 curtail @@
                 let c =
                   let* c = cont c in
                   match c with
                   | GRef (GlobRef.IndRef _ as i, _) ->
                     M.(tell [TRef i]) >> return c
                   | GRef (GlobRef.ConstructRef _ as i, _) ->
                     M.(tell [TRef i]) >> return c
                   | _ -> (* Variables and constants are already dealt with in 'variable' and 'constant' *)
                     return c
                 in
                 let+ c, w = M.listen c in
                 let v = find_variable w in
                 match v with
                 | TVar id -> GVar id
                 | TRef c -> GRef (c, None)
                 | TOther -> GHole (Evar_kinds.GoalEvar, IntroAnonymous, None)
               )
             ; constr_expr = (fun c cont -> M.censor (fun _ -> []) @@ cont c)
             }

let tactic_one_variable t =
  M.run [] @@ FreeVarsMapper.glob_tactic_expr_map mapper t
