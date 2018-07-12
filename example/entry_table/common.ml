open! Core_kernel

(* Things that really we should mostly get from a future Jane_kernel module... *)

module Dir = struct
  type t =
    | Buy
    | Sell
  [@@deriving sexp]

  module Export = struct
    type _dir = t =
      | Buy
      | Sell
  end
end

open Dir.Export

module Dirpair = struct
  type 'a t =
    { buy : 'a
    ; sell : 'a
    }
  [@@deriving sexp, compare]

  module Export = struct
    type 'a _dirpair = 'a t =
      { buy : 'a
      ; sell : 'a
      }
  end

  let to_list t = [ t.buy; t.sell ]

  let get t = function
    | Buy -> t.buy
    | Sell -> t.sell
  ;;

  let fset_dir t (d : Dir.t) v =
    match d with
    | Buy -> { t with buy = v }
    | Sell -> { t with sell = v }
  ;;

  let map_dir t d ~f =
    match d with
    | Buy -> { t with buy = f t.buy }
    | Sell -> { t with sell = f t.sell }
  ;;
end

type bump_dir =
  | Incr
  | Decr
[@@deriving sexp]

type focus_dir =
  | Next
  | Prev
[@@deriving sexp]

let move_map_focus map focus dir =
  match focus with
  | None ->
    let find =
      match dir with
      | Prev -> Map.max_elt
      | Next -> Map.min_elt
    in
    Option.map ~f:fst (find map)
  | Some index ->
    let condition =
      match dir with
      | Prev -> `Less_than
      | Next -> `Greater_than
    in
    (match Map.closest_key map condition index with
     | None -> Some index
     | Some (key, _) -> Some key)
;;
