open! Core_kernel.Std
open! Import

(** Focus management functions  *)

type dir = Prev | Next [@@deriving sexp]

(** Move the focus in the map in the specified direction. Note that the focus doesn't need
    to be a key in the map for this to work.

    If the focus is None, or the current focus is out of view, then moving [Next] moves
    you to the beginning of the visible range, and moving [Prev] puts you at the end of
    the visible range.

    Moving [Next] when you're already focused at the end of the map and [Prev] when you're
    at the beginning of the map does nothing.

    The focus is returned as None only in the case that the map is empty.
*)
val move_focus
  :  ('a,_,_) Map.t
  -> visible_range:('a * 'a) option
  -> focus:'a option
  -> dir
  -> 'a option
