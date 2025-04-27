open! Core
open! Async_kernel

(** For starting up an [Incr_dom] app. *)

(** [start] initializes a new application, including starting up Async and waiting for the
    initial creation of the DOM. [bind_to_element_with_id] determines which element the UI
    will be bound in to. In other words, the DOM returned by [App.view] will replace that
    element.

    The application loop kicked off by [start] runs primarily in response to the
    [request_animation_frame] events kicked off by the browser, but it will also run
    approximately once a second, even if [request_animation_frame] never runs, as is the
    case in some browsers when the pane an application is running in is in the background. *)
val start
  :  ?debug:bool (** print timing info to JS console - default false *)
  -> ?stop:unit Deferred.t
  -> ?named_logging_filters:(string * ('action -> bool)) list
  -> bind_to_element_with_id:string
  -> initial_model:'model
  -> (module App_intf.S with type Model.t = 'model and type Action.t = 'action)
  -> unit

module Private : sig
  val start_bonsai
    :  ?debug:bool (** print timing info to JS console - default false *)
    -> ?stop:unit Deferred.t
    -> ?named_logging_filters:(string * ('action -> bool)) list
    -> ?simulate_body_focus_on_root_element:bool
    -> ?profile:bool
         (** Send timing marks and measures to JS performance buffer - default false *)
    -> bind_to_element_with_id:string
    -> initial_model:'model
    -> (module App_intf.Private.S_for_bonsai
          with type Model.t = 'model
           and type Action.t = 'action)
    -> unit

  val time_source : Ui_time_source.t

  module App_root_registry = App_root_registry
  module Debug_logging = Debug_logging
  module Focus_stealer = Focus_stealer
  module Frame_loop = Frame_loop
  module Performance_measure = Performance_measure

  val warn_if_quirks_mode : unit -> unit
end

module For_profiling : sig
  module Performance_measure = Performance_measure
end

module Private_for_toplayer_to_mutate_inertness : sig
  val connected_app_roots : unit -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t list
end
