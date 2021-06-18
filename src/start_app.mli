open! Core
open! Async_kernel

(** For starting up an [Incr_dom] app. *)

(** [start] initializes a new application, including starting up Async and waiting for the
    initial creation of the DOM.  [bind_to_element_with_id] determines which element the
    UI will be bound in to. In other words, the DOM returned by [App.view] will replace
    that element.

    The application loop kicked off by [start] runs primarily in response to the
    [request_animation_frame] events kicked off by the browser, but it will also run
    approximately once a second, even if [request_animation_frame] never runs, as is the
    case in some browsers when the pane an application is running in is in the background.
*)
val start
  :  ?debug:bool (** print timing info to JS console - default false *)
  -> ?stop:unit Deferred.t
  -> ?named_logging_filters:(string * ('action -> bool)) list
  -> bind_to_element_with_id:string
  -> initial_model:'model
  -> (module App_intf.S with type Model.t = 'model and type Action.t = 'action)
  -> unit
