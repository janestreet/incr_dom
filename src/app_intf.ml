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

  (** If you selectively render certain parts of the model based on what is visible on the
      screen, use [update_visibility] to query the state of the DOM and make the required
      updates in the model.  Otherwise, it can be safely set to the identity function.

      Changes in visibility that cause the page to reflow, thereby causing more changes in
      visibility, must be avoided. In order to make such bugs more visible, cascading
      sequences of [update_visibility] are not prevented.  *)
  val update_visibility : Model.t -> Model.t

  (** [view] sets up the incremental from the model to the virtual DOM.

      Call [schedule] for requesting an action to be applied.

      Call [viewport_changed] when you detect scrolling or resizing of an inner
      pane. Scrolling in the view's scroll container and resizing of the window are
      already tracked by [Start_app].  *)
  val view
    :  Model.t Incr.t
    -> schedule:(Action.t -> unit)
    -> viewport_changed:(unit -> unit)
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

  val update_visibility : Model.t -> Derived_model.t -> Model.t

  val view
    :  Model.t Incr.t
    -> Derived_model.t Incr.t
    -> schedule:(Action.t -> unit)
    -> viewport_changed:(unit -> unit)
    -> Vdom.Node.t Incr.t
end
