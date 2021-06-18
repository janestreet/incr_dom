open! Core
open! Incr_dom
module Id : Identifiable

module Model : sig
  type t =
    { data : string
    ; font_size : int
    }
  [@@deriving sexp_of, fields, compare]

  val create : data:string -> t
end

module Action : sig
  type t =
    | Change_data of string
    | Increase_font
    | Increase_font_by of int
  [@@deriving sexp_of]

  val apply : t -> Model.t -> Model.t
  val should_log : t -> bool
end

val view
  :  Model.t Incr.t
  -> row_id:Id.t
  -> inject:(Action.t -> Vdom.Event.t)
  -> Vdom.Node.t Incr.t
