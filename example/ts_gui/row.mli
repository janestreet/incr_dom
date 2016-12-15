open! Core_kernel.Std
open! Incr_dom.Std

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
  type t = Unfocused | Focused | Editing
  [@@deriving sexp]
end

val apply_action : Action.t -> Model.t -> Model.t

val view
  :  Model.t Incr.t
  -> id:string
  -> mode:Mode.t Incr.t
  -> sort_column:int option
  -> focus_me:Vdom.Event.t
  -> remember_edit:(column:string -> string -> Vdom.Event.t)
  -> Vdom.Node.t Incr.t

val random_rows : int -> Model.t list
