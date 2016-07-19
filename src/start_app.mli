open! Core_kernel.Std
open! Async_kernel.Std

(** This call initializes a new application, including starting up Async and waiting for
    the initial creation of the DOM.  Note that the [('action -> unit)] callbacks below
    are used for scheduling actions to be run in the near future.

    [on_startup] is called once, right after the initial DOM is set to the view that
    corresponds to the [initial_state].  This is useful for doing things like starting up
    async processes that are going to look and and modify the model, by updating the state
    incremental.

    [on_display] is called every time the DOM is updated, with the value of the model just
    before and just after the update.  Things like updating the window scroll naturally go
    here.

    If [bind_to_element_with_id] is specified, the app will be bound to the element with
    the given id. In other words, the DOM returned by [App.view] will replace that
    element. If not specified, the app will be bound to the body element.
*)
val start
  :  ?bind_to_element_with_id : string
  -> initial_state : 'model
  -> on_startup : (schedule:('action -> unit) -> 'model Incr.Var.t -> unit)
  -> on_display : (schedule:('action -> unit) -> 'model -> 'model -> unit)
  -> (module App_intf.S with type Model.t = 'model and type Action.t = 'action)
  -> unit
