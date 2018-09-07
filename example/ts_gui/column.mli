open! Core_kernel
open! Import

(** A column type for use in constructing column-oriented views of a collection of "row"
    objects. *)

type 'row t

val create
  :  name:string
  -> ?group:string
  -> ?sort_by:('b -> Sort_key.t)
  -> ?focus_on_edit:unit
  -> (module Stringable with type t = 'b)
  -> editable:bool
  -> get:('row -> 'b)
  -> set:('row -> 'b -> 'row)
  -> 'row t

val of_field
  :  ('row, 'b) Field.t
  -> ?group:string
  -> ?sort_by:('b -> Sort_key.t)
  -> ?focus_on_edit:unit
  -> (module Stringable with type t = 'b)
  -> editable:bool
  -> 'row t

val name : _ t -> string
val editable : _ t -> bool
val focus_on_edit : _ t -> bool
val get : 'row t -> 'row -> string
val set : 'row t -> 'row -> string -> 'row Or_error.t
val sort_by : 'row t -> 'row -> Sort_key.t
val to_table_widget_column : 'row t -> 'row Ts_table.Column.t
