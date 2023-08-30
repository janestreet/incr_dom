open! Core
open! Incr_dom
open! Import

module Model : sig
  type t [@@deriving compare]

  val columns : t Column.t list
  val matches_pattern : t -> string -> bool
  val apply_edit : t -> column:string -> string -> t
end

module Action : sig
  type t [@@deriving sexp]

  val kick_price : t
  val kick_position : t
end

module Mode : sig
  type t =
    | Unfocused
    | Focused
    | Editing
  [@@deriving sexp]
end

val apply_action : Action.t -> Model.t -> Model.t

val view
  :  Model.t Incr.t
  -> mode:Mode.t Incr.t
  -> sort_columns:int list
  -> focused_column:int option Incr.t
  -> focus_me:unit Vdom.Effect.t
  -> focus_nth_column:(int -> unit Vdom.Effect.t)
  -> remember_edit:(column:string -> string -> unit Vdom.Effect.t)
  -> Row_node_spec.t Incr.t

val random_row : unit -> Model.t
val random_rows : int -> Model.t list
