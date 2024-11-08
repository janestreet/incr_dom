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

module Private : sig
  val start_bonsai
    :  ?debug:bool (** print timing info to JS console - default false *)
    -> ?stop:unit Deferred.t
    -> ?named_logging_filters:(string * ('action -> bool)) list
    -> ?simulate_body_focus_on_root_element:bool
    -> ?profile:bool
         (** Send timing marks and measures to JS performance buffer - default false*)
    -> bind_to_element_with_id:string
    -> initial_model:'model
    -> (module App_intf.Private.S_for_bonsai
          with type Model.t = 'model
           and type Action.t = 'action)
    -> unit

  val time_source : Ui_time_source.t
end

module For_profiling : sig
  module Performance_measure : sig
    type t =
      (* Startup*)
      | Bonsai_graph_application
      | Bonsai_preprocess
      | Bonsai_gather
      | Incr_app_creation
      | First_stabilization
      | Mount_initial_dom
      (* Per-frame*)
      | Whole_animation_frame_loop
      | Stabilize_for_clock
      | Update_visibility
      | Stabilize_for_update_visibility
      | Apply_actions
      | Stabilize_for_action
      | Stabilize_after_all_apply_actions
      | Diff_vdom
      | Patch_vdom
      | On_display_handlers
      | Start_of_frame_to_start_of_next_frame
      | End_of_frame_to_start_of_next_frame
    [@@deriving to_string]

    module For_bonsai_web_start_only : sig
      val timer_start : t -> profile:bool -> unit
      val timer_stop : t -> profile:bool -> unit
    end
  end
end

module For_mutating_inertness : sig
  val app_root_class : string
end
