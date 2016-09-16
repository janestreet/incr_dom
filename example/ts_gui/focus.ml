open! Core_kernel.Std
open! Import

type dir = Prev | Next [@@deriving sexp]

let move_focus (type key)
      (map : (key,_,_) Map.t)
      ~(visible_range: (key * key) option)
      ~(focus : key option)
      dir
  =
  (* Effectively forget the focus if it's not currently in visible range. *)
  let focus =
    let c = Map.comparator map in
    match visible_range with
    | None -> focus
    | Some (lo,hi) ->
      Option.bind focus ~f:(fun key ->
        if c.compare key lo < 0
        || c.compare key hi > 0
        then None
        else (Some key))
  in
  match focus with
  | None ->
    (* If there's no focus, we grab the extreme element, depending on the direction of
       movement. *)
    let default extreme_elt fst_or_snd =
      match visible_range with
      | None -> Option.map (extreme_elt map) ~f:fst
      | Some range -> Some (fst_or_snd range)
    in
    (match dir with
     | Prev -> default Map.max_elt snd
     | Next -> default Map.min_elt fst)
  | Some key ->
    (* If we are focused, then just move to the next key in the map. *)
    let dir = match dir with
      | Prev -> `Less_than
      | Next -> `Greater_than
    in
    (match Map.closest_key map dir key with
     | None -> Some key
     | Some (new_key,_) -> Some new_key)
