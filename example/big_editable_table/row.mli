open! Core_kernel.Std
open! Incr_dom.Std

module Id : sig
  type t
  include Identifiable with type t := t
  val to_dom : t -> string
  val of_int : int -> t
end

module Model : sig
  type t =
    { percentage : float
    ; speed : float
    ; aggro : float
    ; tries : int
    ; owner : string
    ; updates : int
    ; price : float
    ; position : int
    ; height : int
    }
  [@@deriving sexp_of]

  val empty : t
end

module Action : sig
  type t =
    | Set_price of float
    | Set_position of int
    | Change_position of int
    | Set_owner of string
  [@@deriving sexp]

  val apply : t -> Model.t -> Model.t
  val should_log : t -> bool
end

val view
  :  id:Id.t
  -> Model.t Incr.t
  -> focused:bool Incr.t
  -> focus_me:Vdom.Event.t
  -> Vdom.Node.t Incr.t

val header
  :  ?widths:int option list Incr.t
  -> Vdom.Attr.t list Incr.t
  -> Vdom.Node.t Incr.t

val column_names : string list
