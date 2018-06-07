open! Core_kernel
open Virtual_dom
open Async_kernel

(** The interface for a basic, incrementally rendered application. *)
module type S_simple = sig

  module Model : sig
    type t

    (** A function for testing whether the model has changed enough to require refiring
        the incremental graph.

        It's best if the values in the model support a semantically reasonable cutoff
        function which lets you avoid infinite recomputation loops that can otherwise be
        triggered by the visibility checks. For this reason, it's typically a good idea to
        avoid having simple closures stored in the model.

        That said, it does work if you put phys_equal in for the cutoff. *)
    val cutoff : t -> t -> bool
  end

  module Action : sig
    type t [@@deriving sexp_of]
    val should_log : t -> bool
  end

  module State : sig
    (** Represents the imperative state associated with an application, typically used for
        housing things like communication Async-RPC connections. *)
    type t
  end

  (** [apply_action] performs modifications to the model as dictated by the action. *)
  val apply_action
    :  Action.t
    -> Model.t
    -> State.t
    -> schedule_action:(Action.t -> unit)
    -> Model.t

  (** If you selectively render certain parts of the model based on what is visible on the
      screen, use [update_visibility] to query the state of the DOM and make the required
      updates in the model.  Otherwise, it can be safely set to the identity function.

      Changes in visibility that cause the page to reflow, thereby causing more changes in
      visibility, must be avoided. In order to make such bugs more visible, cascading
      sequences of [update_visibility] are not prevented.  *)
  val update_visibility : Model.t -> Model.t

  (** [view] sets up the incremental from the model to the virtual DOM.

      [inject] gives you the ability to create event handlers in the virtual DOM. In your
      event handler, call this function on the action you would like to schedule. Virtual
      DOM will automatically delegate that action back to the [Start_app] main loop. *)
  val view
    :  Model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> Vdom.Node.t Incr.t

  (** [on_startup] is called once, right after the initial DOM is set to the
      view that corresponds to the initial state.  This is useful for doing
      things like starting up async processes. *)
  val on_startup
    :  schedule_action:(Action.t -> unit)
    -> Model.t
    -> State.t Deferred.t

  (** [on_display] is called every time the DOM is updated, with the model just before the
      update and the model just after the update. Use [on_display] to initiate actions. *)
  val on_display
    :  old:Model.t
    -> Model.t
    -> State.t
    -> schedule_action:(Action.t -> unit)
    -> unit
end

(** The interface for a basic, incrementally rendered application with the possibility
    for a mutable model. *)
module type S_imperative = sig
  module Model : sig
    type t
    val cutoff : t -> t -> bool
  end

  module State : sig type t end

  (** [Model_summary] allows you to make the [Model] be mutable, but persist the necessary
      information to discover edge transitions in [on_display]. The old model is persisted
      as a summary after stabilization and is passed to the next invocation of
      [on_display] *)
  module Model_summary : sig
    type t

    (** [create] extracts the information needed to compute edge transitions in
        [on_display]. If [Model.t] is immutable, this can be [Fn.id]. If you don't need to
        trigger external actions, then this can be [Fn.const ()]. If your model is mutable
        and you need to trigger actions, then this will need to be somewhere in between *)
    val create : Model.t -> t
  end

  module Action : sig
    type t [@@deriving sexp_of]

    val should_log : t -> bool
  end

  val apply_action
    :  Action.t
    -> Model.t
    -> State.t
    -> schedule_action:(Action.t -> unit)
    -> Model.t

  val update_visibility : Model.t -> Model.t

  val view
    :  Model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> Vdom.Node.t Incr.t

  val on_startup
    :  schedule_action:(Action.t -> unit)
    -> Model.t
    -> State.t Deferred.t

  val on_display
    :  old:Model_summary.t
    -> Model.t
    -> State.t
    -> schedule_action:(Action.t -> unit)
    -> unit
end

(** An extension of the basic API that supports the use of a derived model. The purpose of
    this is to allow sharing of an incremental computation between the creation of the
    view and the application of an action. *)
module type S_derived = sig
  module Model : sig
    type t
    val cutoff : t -> t -> bool
  end

  (** [Derived_model] is the data container that allows you to share computations between
      the actions and the view. Any things that the actions need to use should be stored
      in Derived_model.t. Then, in [Action.apply], you can call
      [stabilize_and_get_derived] to retrieve that data and make use of it. *)
  module Derived_model : sig
    type t

    (** [create] sets up the incremental that performs the shared computations. Sharing
        computations will typically look something like this:

        {[
          let%map shared1 = computation1
          and     shared2 = computation2
          and     shared3 = computation3
          in
          { shared1; shared2; shared3 }
        ]}
    *)
    val create : Model.t Incr.t -> t Incr.t
  end

  module Model_summary : sig
    type t

    val create : Model.t -> Derived_model.t -> t
  end

  module State : sig type t end

  module Action : sig
    type t [@@deriving sexp_of]

    val should_log : t -> bool
  end

  val apply_action
    :  Action.t
    -> Model.t
    -> State.t
    -> schedule_action:(Action.t -> unit)
    -> recompute_derived:(Model.t -> Derived_model.t)
    -> Model.t

  (** [update_visbility] gives you access to both the model and the derived model.

      If you do some intermediate updates to the model, and would like to recompute the
      derived model from those, [recompute_derived model' => derived'] will give you
      that ability. [recompute_derived] updates the model in the incremental graph and
      re-stabilizes the derived model, before giving it back to you. *)
  val update_visibility
    :  Model.t
    -> Derived_model.t
    -> recompute_derived:(Model.t -> Derived_model.t)
    -> Model.t

  val view
    :  Model.t Incr.t
    -> Derived_model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> Vdom.Node.t Incr.t

  val on_startup
    :  schedule_action:(Action.t -> unit)
    -> Model.t
    -> Derived_model.t
    -> State.t Deferred.t

  val on_display
    :  old:Model_summary.t
    -> Model.t
    -> Derived_model.t
    -> State.t
    -> schedule_action:(Action.t -> unit)
    -> unit
end
