open! Core_kernel.Std
open! Async_kernel.Std

(** This call initializes a new application, including starting up Async and waiting for
    the initial creation of the DOM.  Note that the [('action -> unit)] callbacks below
    are used for scheduling actions to be run in the near future.
*)
val start
  (** If [bind_to_element_with_id] is specified, the app will be bound to the element with
    the given id. In other words, the DOM returned by [App.view] will replace that
    element. If not specified, the app will be bound to the body element. *)
  :  ?bind_to_element_with_id : string

  -> initial_state : 'model

  (** [on_startup] is called once, right after the initial DOM is set to the
      view that corresponds to the [initial_state].  This is useful for doing
      things like starting up async processes that are going to look and and
      modify the model, by updating the state incremental. *)
  -> on_startup
     : (schedule:('action -> unit) -> 'model Incr.Var.t -> unit)

  (** [project_immutable_summary] is called on the model before and after each
      time the DOM is updated to extract an immutable piece. This immutable
      piece is what is passed to the on_display functions. *)
  -> project_immutable_summary
     : ('model -> 'summary)

  (** [on_display] is called every time the DOM is updated, with the immutable
      summary of the model just before the update and the model just after the
      update. Use [on_display] to initiate edge-triggered (as opposed to
      level-triggered) actions. *)
  -> on_display
     : (schedule:('action -> unit) -> old:'summary -> 'model -> unit)

  -> (module App_intf.S
       with type Model.t = 'model
        and type Action.t = 'action)

  -> unit

val start_derived
  :  ?bind_to_element_with_id : string
  -> initial_state : 'model

  -> on_startup
     : (schedule:('action -> unit) -> 'model Incr.Var.t -> unit)

  -> project_immutable_summary
     : ('model -> 'derived -> 'summary)

  -> on_display
     : (schedule:('action -> unit) -> old:'summary -> 'model -> 'derived -> unit)

  -> (module App_intf.S_derived
       with type Model.t = 'model
        and type Derived_model.t = 'derived
        and type Action.t = 'action)

  -> unit
