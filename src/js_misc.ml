open! Core_kernel.Std
open Js_of_ocaml

module Rect = struct
  type 'a t =
    { top    : 'a
    ; left   : 'a
    ; bottom : 'a
    ; right  : 'a
    } [@@deriving sexp, bin_io, compare, fields]
end

type rows_or_columns = Rows | Columns [@@deriving sexp, bin_io, variants, compare]

(* The javascript code for assessing visibility in this module is adapted from:

    http://stackoverflow.com/questions/123999/how-to-tell-if-a-dom-element-is-visible-in-the-current-viewport/7557433#7557433
*)

let element_is_in_viewport :  Dom_html.element Js.t -> bool =
  let f = Js.Unsafe.eval_string {js|
       (function (el) {

          //special bonus for those using jQuery
          if (typeof jQuery === "function" && el instanceof jQuery) {
              el = el[0];
          }

          var rect = el.getBoundingClientRect();

          return (
              rect.top >= 0 &&
              rect.left >= 0 &&
              rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
              rect.right <= (window.innerWidth || document.documentElement.clientWidth)
          );
         })
      |js}
  in
  (fun elt -> Js.Unsafe.fun_call f [| Js.Unsafe.inject elt |] |> Js.to_bool)
;;

(** Scrolls to the item marked as "keep-in-view" *)
let scroll ?(id="keep-in-view") () =
  match Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) with
  | None -> ()
  | Some elt ->
    if not (element_is_in_viewport elt) then
      elt##scrollIntoView (Js.bool true)
;;

(* Not yet supported on Chrome.  Maybe we should use jQuery?
   {[
     let scroll_into_view : Dom_html.element Js.t -> unit =
       let f = Js.Unsafe.eval_string {js|
         (function (el) { el.scrollIntoView({block: "start", behavior: "smooth"}); })
       |js}
       in
       (fun elt -> Js.Unsafe.fun_call f [| Js.Unsafe.inject elt |])
   ]}
*)

(* [viewport_rect_of_element el] gets bounding rect of of [el]. The bounding rect is
   relative to the view port *)
let viewport_rect_of_element : Dom_html.element Js.t -> int Rect.t =
  let f = Js.Unsafe.eval_string {js|
       (function (el) {

          //special bonus for those using jQuery
          if (typeof jQuery === "function" && el instanceof jQuery) {
              el = el[0];
          }

          var rect = el.getBoundingClientRect();

          return [ rect.top, rect.left, rect.bottom, rect.right ]
         })
      |js}
  in
  (fun elt ->
     match Js.Unsafe.fun_call f [| Js.Unsafe.inject elt |] |> Js.to_array with
     | [| top; left; bottom; right |] -> { Rect.top; left; bottom; right }
     | _ -> failwith "viewport_rect_of_element : element not found"
  )
;;

let viewport_rect : unit -> int Rect.t =
  let f = Js.Unsafe.eval_string {js|
       (function () {
          // "||" here means if the left side is not defined then use the right side
          var height = window.innerHeight || document.documentElement.clientHeight;
          var width  = window.innerWidth  || document.documentElement.clientWidth;
          return [ height, width ]
         })
      |js}
  in
  (fun () ->
     match Js.Unsafe.fun_call f [| |] |> Js.to_array with
     | [| bottom; right |] -> { Rect.top = 0; left = 0; bottom; right }
     | _ -> failwith "viewport_rect : unexpected exception"
  )
;;

(** Simple wrapper for the binary-search functor   *)
let binary_search (type elt) ~length ~get ~compare mode x =
  let module Bs =
    Binary_searchable.Make_without_tests(struct
      type nonrec elt = elt
      type nonrec t   = unit

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
      match Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) with
      | None     -> failwithf "Element %s not found" id ()
      | Some elt -> elt
    in
    let first = viewport_rect_of_element (nth_element 0)            in
    let last  = viewport_rect_of_element (nth_element (length - 1)) in
    (* Compute the n'th element in a normalized way, as if the indexing always goes in
       increasing order from top top bottom *)
    let nth_element_normalized n =
      let is_ascending =
        match layout with
        | Rows    -> Int.(<=) first.top  last.top
        | Columns -> Int.(<=) first.left last.left
      in
      nth_element (if is_ascending then n else length - n - 1)
    in
    fun n -> search_by (viewport_rect_of_element (nth_element_normalized n))
  in
  binary_search ~length ~get ~compare:Int.compare mode x
;;

let find_visible_range ~length ~nth_element_id layout =
  if length = 0 then None
  else
    let element_search = element_search ~length ~nth_element_id in
    let viewport_rect = viewport_rect () in
    let first =
      let search_by, target =
        match layout with
        | Rows    -> Rect.bottom , viewport_rect.top
        | Columns -> Rect.right  , viewport_rect.left
      in
      element_search ~search_by `First_strictly_greater_than layout target
    in
    let last =
      let search_by, target =
        match layout with
        | Rows    -> Rect.top  , viewport_rect.bottom
        | Columns -> Rect.left , viewport_rect.right
      in
      element_search ~search_by `Last_strictly_less_than layout target
    in
    (* Both [first] and [last] need to be [Some]. Otherwise, for example if the whole
       table is below view port, then [first] will be [Some], [last] will be [None] *)
    Option.both first last
;;
