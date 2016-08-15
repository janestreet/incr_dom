open! Core_kernel.Std
open! Async_kernel.Std

(** The calls in this module initialize new applications, including starting up Async and
    waiting for the initial creation of the DOM.

    In each of these calls, if [bind_to_element_with_id] is specified, the app will be
    bound to the element with the given id. In other words, the DOM returned by [App.view]
    will replace that element. If not specified, the app will be bound to the body
    element.

    The differences between these calls can be understood in more detail by looking at the
    module type definitions in [App_intf].
*)

val simple
  :  ?bind_to_element_with_id : string
  -> initial_state : 'model
  -> (module App_intf.S_simple with type Model.t = 'model)
  -> unit

val imperative
  :  ?bind_to_element_with_id : string
  -> initial_state : 'model
  -> (module App_intf.S_imperative with type Model.t = 'model)
  -> unit

val derived
  :  ?bind_to_element_with_id : string
  -> initial_state : 'model
  -> (module App_intf.S_derived with type Model.t = 'model)
  -> unit
