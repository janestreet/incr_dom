open! Core_kernel.Std
open Virtual_dom.Std

module type S = sig
  module Model : sig
    type t [@@deriving sexp]
  end

  module Action : sig
    type t [@@deriving sexp]

    val apply :
      t
      -> schedule:(t -> unit)
      -> Model.t
      -> Model.t

    val should_log : t -> bool
  end

  val view
    :  Model.t Incr.t
    -> schedule:(Action.t -> unit)
    -> Vdom.Node.t Incr.t
end
