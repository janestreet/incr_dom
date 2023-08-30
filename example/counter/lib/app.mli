open! Core

module Model : sig
  type t [@@deriving sexp]

  val cutoff : t -> t -> bool
end

module Action : sig
  type t = Increment [@@deriving sexp_of]
end

include
  Incr_dom.App_intf.S
    with type State.t = unit
     and module Model := Model
     and module Action := Action

val initial_model : Model.t
