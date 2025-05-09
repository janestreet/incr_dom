open! Core
open Js_of_ocaml
module Performance = Javascript_profiling

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
[@@deriving
  to_string ~capitalize:"lower sentence case", enumerate, hash, compare, sexp_of]

let clear_all_from_browser_buffer () =
  List.iter all ~f:(fun t -> Performance.clear_measures ~name:(to_string t) ())
;;

type timer =
  { timer : Performance.Timer.t
  ; event : t
  ; profile : bool
  ; debug : bool
  }

let timer_start t ~debug ~profile =
  if debug then Console.console##time (Js.string (to_string t));
  { timer = Performance.Timer.start (); event = t; profile; debug }
;;

let timer_stop { timer; event; profile; debug } =
  if profile
  then (
    let measurement = Performance.Timer.stop timer in
    match Performance.Timer.duration measurement with
    | Backgrounding_changed_unreliable _ -> ()
    | Ok duration ->
      let observe_hist x = Ui_metrics.Timing_histograms.observe x duration in
      let observe_one_off x = Ui_metrics.One_off_timings.observe x duration in
      let observe_hist_and_annotate ?color x =
        observe_hist x;
        Performance.measure ?color (to_string event) measurement
      in
      let observe_one_off_and_annotate ?color x =
        observe_one_off x;
        Performance.measure ?color (to_string event) measurement
      in
      (* The color mapping is arbitrary, but roughly:
          - Bonsai is secondary, vdom is tertiary
          - Light should draw less attention, dark should draw more. *)
      (match event with
       | Bonsai_graph_application ->
         observe_one_off_and_annotate ~color:Secondary Bonsai_graph_application
       | Bonsai_preprocess ->
         observe_one_off_and_annotate ~color:Secondary_light Bonsai_preprocess
       | Bonsai_gather -> observe_one_off_and_annotate ~color:Secondary_dark Bonsai_gather
       | Incr_app_creation ->
         observe_one_off_and_annotate ~color:Secondary_light Incr_app_creation
       | First_stabilization ->
         observe_one_off_and_annotate ~color:Secondary_dark First_stabilization
       | Mount_initial_dom ->
         observe_one_off_and_annotate ~color:Tertiary Mount_initial_dom
       | Whole_animation_frame_loop -> observe_hist Bonsai_whole_frame_loop
       | Stabilize_for_clock ->
         observe_hist_and_annotate ~color:Secondary_light Bonsai_stabilization_clock
       | Update_visibility ->
         observe_hist_and_annotate ~color:Secondary_light Bonsai_update_visibility
       | Stabilize_for_update_visibility ->
         observe_hist_and_annotate
           ~color:Secondary_light
           Bonsai_stabilization_update_visibility
       | Apply_actions -> observe_hist_and_annotate ~color:Secondary Bonsai_apply_action
       | Stabilize_for_action ->
         observe_hist_and_annotate ~color:Secondary_dark Bonsai_stabilization_action
       | Stabilize_after_all_apply_actions ->
         observe_hist_and_annotate
           ~color:Secondary_dark
           Bonsai_stabilization_after_apply_actions
       | Diff_vdom -> observe_hist_and_annotate ~color:Tertiary Bonsai_diff_vdom
       | Patch_vdom -> observe_hist_and_annotate ~color:Tertiary_dark Bonsai_patch_vdom
       | On_display_handlers ->
         observe_hist_and_annotate ~color:Secondary Bonsai_display_handlers
       | Start_of_frame_to_start_of_next_frame ->
         observe_hist Bonsai_start_of_frame_to_start_of_next_frame
       | End_of_frame_to_start_of_next_frame ->
         observe_hist Bonsai_end_of_frame_to_start_of_next_frame));
  if debug then Console.console##timeEnd (Js.string (to_string event))
;;

let time t ~debug ~profile ~f =
  let timer = timer_start t ~debug ~profile in
  let result = f () in
  timer_stop timer;
  result
;;
