open! Core_kernel.Std
open! Incr_dom.Std

module Field : sig
  type t = Percentage | Speed | Aggro | Tries [@@deriving sexp]
  val to_dom : t -> string
end

module Model : sig
  type t =
    { percentage : string
    ; speed : string
    ; aggro : string
    ; tries : string
    ; focus : Field.t
    } [@@deriving sexp_of, fields]

  val apply : t -> Row.Model.t -> Row.Model.t option
  val empty : t
end

module Action : sig
  type t =
    | Value of Field.t * string
    | Move_focus of [ `Left | `Right ]
  [@@deriving sexp]

  val apply : t -> Model.t -> Model.t
  val should_log : t -> bool
end

val view
  :  Model.t Incr.t
  -> inject:(Action.t -> Vdom.Event.t)
  -> Vdom.Node.t Incr.t
