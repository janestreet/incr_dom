open! Core

module Model : sig
  type t [@@deriving sexp]

  val cutoff : t -> t -> bool
end

include Incr_dom.App_intf.S with type State.t = unit and module Model := Model

val initial_model : Model.t
