open CMap

module Make(M : Map.OrderedType)
    (Map : ExtS with
      type key = M.t
                 and type 'a t = 'a Map.Make(M).t
                 and module Set := Set.Make(M)
    ) : sig
  val symmetric_diff : ([ `Left of M.t | `Right of M.t ] -> 'b -> 'b) -> 'a Map.t -> 'a Map.t -> 'b -> 'b
end = struct

  [@@@warning "-37"]
  type 'a map =
    | MEmpty
    | MNode of {l:'a Map.t; v:M.t; d:'a; r:'a Map.t; h:int}
  [@@@warning "+37"]

  let map_prj : 'a Map.t -> 'a map = Obj.magic

  type 'a sequenced =
    | End
    | More of M.t * 'a Map.t * 'a sequenced

  let rec seq_cons m rest =
    match map_prj m with
    | MEmpty -> rest
    | MNode {l; v; r; _ } -> seq_cons l (More (v, r, rest))

  let rec fold_seq f acc = function
    | End -> acc
    | More (k, m, r) -> f k @@ fold_seq f (Map.fold (fun k _ acc -> f k acc) m acc) r

  let move_to_acc (m, acc) = match map_prj m with
    | MEmpty -> assert false
    | MNode {l; v; r; _ } -> l, More (v, r, acc)

  let rec symmetric_cons ((lm, la) as l) ((rm, ra) as r) =
    if lm = rm then la, ra
    else
      let lh = Map.height lm in
      let rh = Map.height rm in
      if lh == rh then
        symmetric_cons (move_to_acc l) (move_to_acc r)
      else if lh < rh then
        symmetric_cons l (move_to_acc r)
      else
        symmetric_cons (move_to_acc l) r

  let symmetric_diff f lm rm acc =
    let rec aux s acc =
      match s with
      | End, rs -> fold_seq (fun k -> f (`Right k)) acc rs
      | ls, End -> fold_seq (fun k -> f (`Left k)) acc ls
      | (More (kl, tl, rl) as ls), (More (kr, tr, rr) as rs) ->
        let cmp = M.compare kl kr in
        if cmp == 0 then
          if tl == tr then
            aux (rl, rr) acc
          else
            aux (symmetric_cons (tl, rl) (tr, rr)) acc
        else if cmp < 0 then
          f (`Left kl) @@ aux (seq_cons tl rl, rs) acc
        else
          f (`Right kr) @@ aux (ls, seq_cons tr rr) acc
    in aux (symmetric_cons (lm, End) (rm, End)) acc
  end
