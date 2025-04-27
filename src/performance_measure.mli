open! Core

(** [Performance_measure] provides timer functions for measuring various parts of the
    Bonsai / Incr_dom main loop. *)

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

(** [clear_all_from_browser_buffer] will clear recorded measurements from the browser's
    performance buffer. This will not affect the Performance Timeline in the DevTools
    panel (if in use), or apps pulling new data from a [PerformanceObserver].

    Apps recording a lot of performance events should do this occasionally to avoid a
    memory leak. *)
val clear_all_from_browser_buffer : unit -> unit

type timer

val timer_start : t -> debug:bool -> profile:bool -> timer
val timer_stop : timer -> unit
val time : t -> debug:bool -> profile:bool -> f:(unit -> 'a) -> 'a
