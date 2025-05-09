open! Core
open Virtual_dom
open Async_kernel
open Js_of_ocaml

module Private_for_toplayer_to_mutate_inertness = struct
  let connected_app_roots = App_root_registry.connected_app_roots
end

(** [Visibility] encapsulates the dirtying and cleaning of the visibility flag

    The viewport starts out dirty. When we look at the DOM to compute what is visible by
    calling [update_visibility], the viewport then becomes clean. Any time the user
    scrolls our page or resizes the window, the viewport becomes dirty again. If we update
    the DOM, the viewport becomes dirty again because a DOM update could cause a reflow,
    moving the elements that are visible.

    We have implemented this as a flag instead of requiring the users to send an action on
    scroll because it would make no sense to compute the visibility on the virtual-dom
    when the virtual-dom does not match the actual dom (as it is in the middle of
    [apply_actions]). *)
module Visibility : sig
  type t

  val create_as_dirty : unit -> t
  val mark_clean : t -> unit
  val mark_dirty : t -> unit
  val is_dirty : t -> bool
end = struct
  type t = { mutable when_dirty : unit Ivar.t }

  let create_as_dirty () = { when_dirty = Ivar.create_full () }
  let mark_dirty t = Ivar.fill_if_empty t.when_dirty ()
  let is_dirty t = Ivar.is_full t.when_dirty
  let mark_clean t = if is_dirty t then t.when_dirty <- Ivar.create ()
end

(* If the [<!DOCTYPE html>] declaration is missing, or not at the beginning of the page,
   browsers will use "Quirks Mode", which emulates IE5:
   https://developer.mozilla.org/en-US/docs/Web/API/Document/compatMode

   This can lead to tricky, hard-to-detect bugs. *)
let warn_if_quirks_mode () =
  let document : < compatMode : Js.js_string Js.t Js.prop > Js.t =
    Js_of_ocaml.Js.Unsafe.coerce Dom_html.document
  in
  let compat_mode = Js.to_string document##.compatMode in
  if String.equal compat_mode "BackCompat"
  then
    Console.console##warn
      (Js.string
         "The browser is currently using `Quirks Mode`, which may cause your app to \
          misbehave. To use `Standards Mode`, make sure your HTML file starts with a \
          valid `<!DOCTYPE html>` declaration.")
;;

let time_source : Ui_time_source.t = Ui_time_source.create ~start:(Time_ns.now ())

let start_bonsai
  (type model action)
  ?(debug = false)
  ?(stop = Deferred.never ())
  ?(named_logging_filters = [])
  ?(simulate_body_focus_on_root_element = true)
  ?(profile = false)
  ~bind_to_element_with_id
  ~initial_model
  (module App : App_intf.Private.S_for_bonsai
    with type Model.t = model
     and type Action.t = action)
  =
  let focus_stealer = Focus_stealer.create ~enabled:simulate_body_focus_on_root_element in
  (* [Async_js] init is idempotent and so fine to do. *)
  Async_js.init ();
  if profile
  then
    Async_kernel.Clock_ns.every ~stop (Time_ns.Span.of_int_sec 2) (fun () ->
      Performance_measure.clear_all_from_browser_buffer ());
  don't_wait_for
    (let%bind () = Async_js.document_loaded () in
     warn_if_quirks_mode ();
     let model_v = Incr.Var.create initial_model in
     let model = Incr.Var.watch model_v in
     let model_from_last_display_v = Incr.Var.create initial_model in
     let model_from_last_display = Incr.Var.watch model_from_last_display_v in
     let cutoff =
       Incr.Cutoff.create (fun ~old_value ~new_value ->
         App.Model.cutoff old_value new_value)
     in
     Incr.set_cutoff model cutoff;
     Incr.set_cutoff model_from_last_display cutoff;
     let action_queue = Queue.create () in
     let module Event =
       Vdom.Effect.Define (struct
         module Action = App.Action

         let handle action = Queue.enqueue action_queue action
       end)
     in
     let visibility = Visibility.create_as_dirty () in
     let viewport_changed () = Visibility.mark_dirty visibility in
     (* This registers the [viewport_changed] handler with Virtual_dom. If event handlers
        use the [Vdom.Effect.Viewport_changed] event, we are notified. *)
     let module _ =
       Vdom.Effect.Define_visibility (struct
         let handle = viewport_changed
       end)
     in
     let get_view, get_apply_action, get_update_visibility, get_on_display =
       let obs =
         Performance_measure.time Incr_app_creation ~profile ~debug:false ~f:(fun () ->
           Incr.observe
             (App.create model ~old_model:model_from_last_display ~inject:Event.inject))
       in
       let fetch (f : _ App_intf.Private.snapshot -> _) () =
         f (Incr.Observer.value_exn obs)
       in
       ( fetch (fun { view; _ } -> view)
       , fetch (fun { apply_action; _ } -> apply_action)
       , fetch (fun { update_visibility; _ } -> update_visibility)
       , fetch (fun { on_display; _ } -> on_display) )
     in
     Performance_measure.time First_stabilization ~profile ~debug:false ~f:(fun () ->
       Incr.stabilize ());
     App.on_stabilize ();
     let { Debug_logging.Flags.should_profile; should_debug }, action_logging =
       Debug_logging.init_app
         ~app_id:bind_to_element_with_id
         ~named_logging_filters
         ~initial_debug:debug
     in
     upon stop (fun () -> Debug_logging.cleanup_app ~app_id:bind_to_element_with_id);
     let timer_start s =
       Performance_measure.timer_start
         s
         ~debug:(should_debug ())
         ~profile:(profile || should_profile ())
     in
     let time (type a) evt ~(f : unit -> a) : a =
       Performance_measure.time
         evt
         ~debug:(should_debug ())
         ~profile:(profile || should_profile ())
         ~f
     in
     let html, html_dom =
       time Mount_initial_dom ~f:(fun () ->
         let html = get_view () in
         let html_dom =
           Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
             let html_dom = Vdom.Node.to_dom html in
             let elem = Dom_html.getElementById_exn bind_to_element_with_id in
             let parent = Option.value_exn (Js.Opt.to_option elem##.parentNode) in
             Dom.replaceChild parent html_dom elem;
             html_dom)
         in
         html, html_dom)
     in
     (* we make sure to call [viewport_changed] whenever the window resizes or the scroll
        container in which our HTML is located is scrolled. *)
     let call_viewport_changed_on_event event_name where =
       ignore
         (Dom.addEventListener
            where
            (Dom.Event.make event_name)
            (Dom.handler (fun _ ->
               viewport_changed ();
               Js._true))
            Js._false
          : Dom.event_listener_id)
     in
     call_viewport_changed_on_event "scroll" (Js_misc.get_scroll_container html_dom);
     call_viewport_changed_on_event "resize" Dom_html.window;
     let%bind state =
       App.on_startup
         ~schedule_action:(fun a -> Ui_effect.Expert.handle (Event.inject a))
         (Incr.Var.value model_v)
     in
     let prev_html = ref html in
     let prev_elt = ref html_dom in
     Focus_stealer.maybe_refocus_on_blur focus_stealer prev_elt;
     let app_root_handle = App_root_registry.register_app_root prev_elt in
     upon stop (fun () -> App_root_registry.unregister_app_root app_root_handle);
     let update_visibility () =
       Visibility.mark_clean visibility;
       let new_model =
         (get_update_visibility ())
           ~schedule_event:Ui_effect.Expert.handle
           (Incr.Var.latest_value model_v)
       in
       Incr.Var.set model_v new_model;
       time Stabilize_for_update_visibility ~f:(fun () -> Incr.stabilize ());
       App.on_stabilize ()
     in
     (* It's important that we don't call [Incr.Var.set] within [apply_action] unless
        we're also going to stabilize. Some code in Bonsai relies on this assumption
        as part of its [action_requires_stabilization] logic. Breaking this invariant
        won't break Bonsai code, but it will effectively remove an optimization. *)
     let apply_action action model =
       Debug_logging.maybe_log_action
         action_logging
         ~sexp_of_action:App.Action.sexp_of_t
         action;
       if App.action_requires_stabilization action
       then (
         Incr.Var.set model_v model;
         time Stabilize_for_action ~f:(fun () -> Incr.stabilize ());
         App.on_stabilize ())
       else (
         Ui_metrics.Counters.observe Incr_skipped_stabilizations;
         if should_debug ()
         then Console.console##debug (Js.string "action applied without stabilizing"));
       let new_model =
         (get_apply_action ()) state ~schedule_event:Ui_effect.Expert.handle model action
       in
       App.on_action_application action;
       new_model
     in
     let rec apply_actions model =
       match Queue.dequeue action_queue with
       | None -> Incr.Var.set model_v model
       | Some action ->
         let new_model = apply_action action model in
         apply_actions new_model
     in
     let start_of_frame_to_start_of_next_frame_timer = ref None in
     let end_of_frame_to_start_of_next_frame_timer = ref None in
     let perform_update () =
       Option.iter
         !start_of_frame_to_start_of_next_frame_timer
         ~f:Performance_measure.timer_stop;
       start_of_frame_to_start_of_next_frame_timer
       := timer_start Start_of_frame_to_start_of_next_frame |> Some;
       Option.iter
         !end_of_frame_to_start_of_next_frame_timer
         ~f:Performance_measure.timer_stop;
       let animation_frame_loop_timer = timer_start Whole_animation_frame_loop in
       time Stabilize_for_clock ~f:(fun () ->
         (match am_running_test with
          | false ->
            (* The clock is set only once per call to perform_update, so that all actions that
               occur before each display update occur "at the same time." *)
            let now =
              let date = new%js Js.date_now in
              Time_ns.Span.of_ms (Js.to_float date##getTime)
              |> Time_ns.of_span_since_epoch
            in
            Incr.Clock.advance_clock Incr.clock ~to_:now;
            App.advance_clock_to now
          | true ->
            (* In tests, the testee is responsible for advancing the clock (if needed). *)
            ());
         Incr.stabilize ());
       App.on_stabilize ();
       time Update_visibility ~f:(fun () ->
         if Visibility.is_dirty visibility then update_visibility ());
       time Apply_actions ~f:(fun () -> apply_actions (Incr.Var.value model_v));
       time Stabilize_after_all_apply_actions ~f:(fun () -> Incr.stabilize ());
       App.on_stabilize ();
       let html = get_view () |> Focus_stealer.maybe_wrap_root_element focus_stealer in
       let patch =
         time Diff_vdom ~f:(fun () ->
           Vdom.Node.Patch.create ~previous:!prev_html ~current:html)
       in
       if not (Vdom.Node.Patch.is_empty patch) then Visibility.mark_dirty visibility;
       time Patch_vdom ~f:(fun () ->
         Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
           let elt = Vdom.Node.Patch.apply patch !prev_elt in
           (* [!prev_elt] should almost always refer to the current app root element, and
              be connected. The exceptions are if something external has mutated DOM,
              which we can't control, and if a patch changes the tag of the root element,
              forcing it to be replaced rather than updated.

              To protect against the latter case, we update the [prev_elt] ref immediately
              after patching, before any [on_mount] handlers or lifecycles run. *)
           prev_html := html;
           prev_elt := elt));
       time On_display_handlers ~f:(fun () ->
         (get_on_display ()) state ~schedule_event:Ui_effect.Expert.handle);
       Incr.Var.set model_from_last_display_v (Incr.Var.value model_v);
       if should_debug () then Console.console##debug (Js.string "-------");
       (* Restoring focus from the [<body />] to the app root should mostly be handled by
          the [blur] listener above, but we additionally run this check every frame because:

          - We want the root element to start out focused, so perform an initial
          update/render, then immediately focus the root (unless a non-body element
          already has focus).
          - [blur] doesn't run if the currently focused element is removed from the DOM,
          so we might need to possibly focus-steal after every frame.

          We still want [refocus_on_blur], so that we can respond immediately to most blurs
          without waiting ~16ms for the next frame.
       *)
       Focus_stealer.maybe_refocus_root_element focus_stealer !prev_elt;
       Performance_measure.timer_stop animation_frame_loop_timer;
       end_of_frame_to_start_of_next_frame_timer
       := timer_start End_of_frame_to_start_of_next_frame |> Some
     in
     Frame_loop.start_looping
       ~is_stopped:(fun () -> Deferred.is_determined stop)
       ~perform_update;
     Deferred.never ())
;;

module Private = struct
  let start_bonsai = start_bonsai
  let time_source = time_source

  module App_root_registry = App_root_registry
  module Debug_logging = Debug_logging
  module Focus_stealer = Focus_stealer
  module Frame_loop = Frame_loop
  module Performance_measure = Performance_measure

  let warn_if_quirks_mode = warn_if_quirks_mode
end

module For_profiling = struct
  module Performance_measure = Performance_measure
end

let start
  (type model action)
  ?(debug = false)
  ?(stop = Deferred.never ())
  ?(named_logging_filters = [])
  ~bind_to_element_with_id
  ~initial_model
  (module App : App_intf.S with type Model.t = model and type Action.t = action)
  =
  start_bonsai
    ~debug
    ~stop
    ~named_logging_filters
    ~bind_to_element_with_id
    ~initial_model
    (module struct
      include App

      let action_requires_stabilization _ = true
      let on_action_application _ = ()
      let on_stabilize () = ()

      let advance_clock_to to_ =
        Ui_time_source.advance_clock time_source ~to_;
        Ui_time_source.Private.flush time_source
      ;;

      let create model ~old_model ~inject =
        let open Incr.Let_syntax in
        let%map component = create model ~old_model ~inject in
        let view = Component.view component in
        let apply_action state ~schedule_event _model action =
          let schedule_action a = schedule_event (inject a) in
          Component.apply_action component action state ~schedule_action
        in
        let update_visibility _model ~schedule_event =
          let schedule_action a = schedule_event (inject a) in
          Component.update_visibility component ~schedule_action
        in
        let on_display state ~schedule_event =
          let schedule_action a = schedule_event (inject a) in
          Component.on_display component state ~schedule_action
        in
        { App_intf.Private.view; apply_action; update_visibility; on_display }
      ;;
    end)
;;
