open! Core_kernel
open! Async_kernel

(** For starting up an [Incr_dom] app. *)

(** [start] initializes a new application, including starting up Async and waiting for the
    initial creation of the DOM.  [bind_to_element_with_id] determines which element the
    UI will be bound in to. In other words, the DOM returned by [App.view] will replace
    that element.
*)
val start
  :  ?debug:bool (* print info to JS console - default false *)
  -> ?stop:unit Deferred.t
  -> bind_to_element_with_id:string
  -> initial_model:'model
  -> (module App_intf.S with type Model.t = 'model)
  -> unit

(** Like [start], but makes the specification of the element to be bound to optional,
    which means that by default, it's bound to the body, which turns out to be a bad
    choice.  This function will eventually be eliminated, once we remove the existing
    use-cases. *)
val component_old_do_not_use
  :  ?bind_to_element_with_id:string
  -> ?debug:bool (* print info to JS console - default false *)
  -> ?stop:unit Deferred.t
  -> initial_model:'model
  -> (module App_intf.S with type Model.t = 'model)
  -> unit
