open! Core_kernel.Std
open Js_of_ocaml

(** This module has a small collection of helpful bits of javascript that have no other
    obvious home.  *)

module Rect : sig
  type 'a t =
    { top    : 'a
    ; left   : 'a
    ; bottom : 'a
    ; right  : 'a
    } [@@deriving sexp, bin_io, compare, fields]

  val height : int t -> int
  val width : int t -> int
end

(** [viewport_rect ()] gives you the rectangle that corresponds to the size of the entire
    browser window *)
val viewport_rect : unit -> int Rect.t

(** [viewport_rect_of_element el] gives you the bounding box rectangle for a single
    element *)
val viewport_rect_of_element : Dom_html.element Js.t -> int Rect.t

(** Returns true iff the element in question is in view. *)
val element_is_in_viewport : Dom_html.element Js.t -> bool

(** Find an element with an id (default as "keep-in-view"), and, if it exists, scrolls the
    UI so that that element is in view. *)
val scroll : ?id:string -> unit -> unit

type rows_or_columns = Rows | Columns [@@deriving sexp, bin_io, variants, compare]

(** [find_visible_range ~length ~nth_element_id layout] is useful to find visible rows or
    columns of a table. The time cost is O(log(length)). It assumes the rows/columns are
    non-overlapping and arranged monotonically, though the order doesn't matter.  It's
    assumed that the rows are laid out from [0] to [length - 1].

    If no row is visible, then [None] is returned.  Otherwise, the inclusive upper and
    lower bounds of the visible rows are returned.
*)
val find_visible_range
  :  length:int
  -> nth_element_id:(int -> string)
  -> rows_or_columns
  -> (int * int) option

(** [scroll_container node] finds the closest scrollable ancestor in the DOM tree.
    If there is no scrollable element above the node passed in, then the document
    will be returned
*)
val get_scroll_container : Dom.node Js.t -> Dom.node Js.t
