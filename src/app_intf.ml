open! Core_kernel.Std
open Virtual_dom.Std

(** The interface for a basic, incrementally rendered application. *)
module type S = sig
  module Model : sig
    type t
  end

  module Action : sig
    type t [@@deriving sexp_of]

    val apply
      :  t
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

(** An extension of the basic API that supports the use of a derived model. The purpose of
    this is to allow sharing of an incremental computation between the calculation of the
    view and the application of an action. *)
module type S_derived = sig
  module Model : sig
    type t
  end

  module Derived_model : sig
    type t

    val create : Model.t Incr.t -> t Incr.t
  end

  module Action : sig
    type t [@@deriving sexp_of]

    val apply
      :  t
      -> schedule:(t -> unit)
      -> Model.t
      -> stabilize_and_get_derived:(unit -> Derived_model.t)
      -> Model.t

    val should_log : t -> bool
  end

  val view
    :  Model.t Incr.t
    -> Derived_model.t Incr.t
    -> schedule:(Action.t -> unit)
    -> Vdom.Node.t Incr.t
end

