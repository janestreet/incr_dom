open! Core
open Js_of_ocaml

module Rect = struct
  type 'a t =
    { top : 'a
    ; left : 'a
    ; bottom : 'a
    ; right : 'a
    }
  [@@deriving sexp, bin_io, compare, fields]

  let map t ~f =
    { top = f t.top; left = f t.left; bottom = f t.bottom; right = f t.right }
  ;;

  let int_height t = t.bottom - t.top
  let int_width t = t.right - t.left
  let float_height t = t.bottom -. t.top
  let float_width t = t.right -. t.left
end

let round_float_rect ?(round = Float.iround_nearest_exn) = Rect.map ~f:round

type rows_or_columns =
  | Rows
  | Columns
[@@deriving sexp, bin_io, variants, compare]

let innerHeight () =
  Dom_html.window##.innerHeight
;;

let innerWidth () =
  Dom_html.window##.innerWidth
;;

let element_is_in_viewport (elt : Dom_html.element Js.t) =
  let rect = elt##getBoundingClientRect in
  Float.( >= ) rect##.top 0.
  && Float.( >= ) rect##.left 0.
  && Float.( <= ) rect##.bottom (Float.of_int (innerHeight ()))
  && Float.( <= ) rect##.right (Float.of_int (innerWidth ()))
;;

(** Scrolls to the item marked as "keep-in-view" *)
let scroll ?(id = "keep-in-view") () =
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some elt -> if not (element_is_in_viewport elt) then elt##scrollIntoView Js._true
;;

(* Not yet supported on Chrome.  Maybe we should use jQuery?
   {[
     let scroll_into_view (elt : Dom_html.element Js.t) : unit =
       elt##scrollIntoView (object%js
         val block = Js.string "start"
         val behavior = Js.string "smooth"
       end)
   ]}
*)
(* [viewport_rect_of_element el] gets bounding rect of [elt]. The bounding rect is
   relative to the view port *)
let viewport_rect_of_element (elt : Dom_html.element Js.t) : float Rect.t =
  let rect = elt##getBoundingClientRect in
  { Rect.top = rect##.top
  ; left = rect##.left
  ; bottom = rect##.bottom
  ; right = rect##.right
  }
;;

let viewport_rect () =
  { Rect.top = 0; left = 0; bottom = innerHeight (); right = innerWidth () }
;;

let client_rect_of_element (elt : Dom_html.element Js.t) : float Rect.t =
  let bounding_rect = viewport_rect_of_element elt in
  { bounding_rect with
    bottom = bounding_rect.top +. Float.of_int elt##.clientHeight
  ; right = bounding_rect.left +. Float.of_int elt##.clientWidth
  }
;;

let client_rect () =
  { Rect.top = 0
  ; left = 0
  ; bottom = Dom_html.document##.documentElement##.clientHeight
  ; right = Dom_html.document##.documentElement##.clientWidth
  }
;;


(** Simple wrapper for the binary-search functor   *)
let binary_search (type elt) ~length ~get ~compare mode x =
  let module Bs =
    Binary_searchable.Make (struct
      type nonrec elt = elt
      type nonrec t = unit

      let get () n = get n
      let length () = length
    end)
  in
  Bs.binary_search () ~compare mode x
;;

(** Searches through elements indexed from [0] to [length - 1]. *)
let element_search ~length ~nth_element_id ~search_by mode layout x =
  let get =
    let nth_element n =
      let id = nth_element_id n in
      match Dom_html.getElementById_opt id with
      | None -> failwithf "Element %s not found" id ()
      | Some elt -> elt
    in
    let first = viewport_rect_of_element (nth_element 0) in
    let last = viewport_rect_of_element (nth_element (length - 1)) in
    (* Compute the n'th element in a normalized way, as if the indexing always goes in
       increasing order from top top bottom *)
    let nth_element_normalized n =
      let is_ascending =
        match layout with
        | Rows -> Float.( <= ) first.top last.top
        | Columns -> Float.( <= ) first.left last.left
      in
      nth_element (if is_ascending then n else length - n - 1)
    in
    fun n -> search_by (viewport_rect_of_element (nth_element_normalized n))
  in
  binary_search ~length ~get ~compare:Float.compare mode x
;;

let find_visible_range ~length ~nth_element_id layout =
  if length = 0
  then None
  else (
    let element_search = element_search ~length ~nth_element_id in
    let viewport_rect = viewport_rect () in
    let first =
      let search_by, target =
        match layout with
        | Rows -> Rect.bottom, viewport_rect.top
        | Columns -> Rect.right, viewport_rect.left
      in
      element_search ~search_by `First_strictly_greater_than layout (Float.of_int target)
    in
    let last =
      let search_by, target =
        match layout with
        | Rows -> Rect.top, viewport_rect.bottom
        | Columns -> Rect.left, viewport_rect.right
      in
      element_search ~search_by `Last_strictly_less_than layout (Float.of_int target)
    in
    (* Both [first] and [last] need to be [Some]. Otherwise, for example if the whole
       table is below view port, then [first] will be [Some], [last] will be [None] *)
    Option.both first last)
;;

let get_scroll_container_js_expr =
  Js.Unsafe.pure_js_expr
    {js|
      (function (element) {
        var doc = element.ownerDocument || document;
        var win = doc.defaultView || window;
        var re = /(auto|scroll)/;

        if (element === doc) {
          return doc;
        }

        var cur = element.parentNode;

        while (cur.parentNode) {
          var style = win.getComputedStyle(cur);

          if (re.test(style.overflow + style.overflowY + style.overflowX)) {
            return cur;
          }

          cur = cur.parentNode;
        }

        return doc;
      })
    |js}
;;

let get_scroll_container (el : #Dom.node Js.t) : Dom.node Js.t =
  Js.Unsafe.fun_call get_scroll_container_js_expr [| Js.Unsafe.inject el |]
;;
