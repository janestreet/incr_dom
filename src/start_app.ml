open! Core
open Virtual_dom
open Async_kernel
open Js_of_ocaml
module Performance = Javascript_profiling

module For_mutating_inertness = struct
  let app_root_class = "private-app-root-for-inertness"
end

module Performance_measure = struct
  module T = struct
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
  end

  include T

  let started_timers = Hashtbl.create (module T)

  let clear_associated_marks_and_measures () =
    List.iter all ~f:(fun t -> Performance.clear_measures ~name:(to_string t) ())
  ;;

  let timer_start t ~debug ~profile =
    if profile then Hashtbl.set started_timers ~key:t ~data:(Performance.Timer.start ());
    if debug then Firebug.console##time (Js.string (to_string t))
  ;;

  let timer_stop t ~debug ~profile =
    (match profile, Hashtbl.find started_timers t with
     | true, Some timer ->
       let measurement = Performance.Timer.stop timer in
       (match Performance.Timer.duration measurement with
        | Backgrounding_changed_unreliable _ -> ()
        | Ok duration ->
          let observe_hist x = Bonsai_metrics.Timing_histograms.observe x duration in
          let observe_one_off x = Bonsai_metrics.One_off_timings.observe x duration in
          let observe_hist_and_annotate ?color x =
            observe_hist x;
            Performance.measure ?color (to_string t) measurement
          in
          let observe_one_off_and_annotate ?color x =
            observe_one_off x;
            Performance.measure ?color (to_string t) measurement
          in
          (* The color mapping is arbitrary, but roughly:
          - Bonsai is secondary, vdom is tertiary
          - Light should draw less attention, dark should draw more. *)
          (match t with
           | Bonsai_graph_application ->
             observe_one_off_and_annotate ~color:Secondary Bonsai_graph_application
           | Bonsai_preprocess ->
             observe_one_off_and_annotate ~color:Secondary_light Bonsai_preprocess
           | Bonsai_gather ->
             observe_one_off_and_annotate ~color:Secondary_dark Bonsai_gather
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
           | Apply_actions ->
             observe_hist_and_annotate ~color:Secondary Bonsai_apply_action
           | Stabilize_for_action ->
             observe_hist_and_annotate ~color:Secondary_dark Bonsai_stabilization_action
           | Stabilize_after_all_apply_actions ->
             observe_hist_and_annotate
               ~color:Secondary_dark
               Bonsai_stabilization_after_apply_actions
           | Diff_vdom -> observe_hist_and_annotate ~color:Tertiary Bonsai_diff_vdom
           | Patch_vdom ->
             observe_hist_and_annotate ~color:Tertiary_dark Bonsai_patch_vdom
           | On_display_handlers ->
             observe_hist_and_annotate ~color:Secondary Bonsai_display_handlers
           | Start_of_frame_to_start_of_next_frame ->
             observe_hist Bonsai_start_of_frame_to_start_of_next_frame
           | End_of_frame_to_start_of_next_frame ->
             observe_hist Bonsai_end_of_frame_to_start_of_next_frame))
     | false, _ | true, None -> ());
    if debug then Firebug.console##timeEnd (Js.string (to_string t))
  ;;

  module For_bonsai_web_start_only = struct
    let timer_start = timer_start ~debug:false
    let timer_stop = timer_stop ~debug:false
  end
end

let print_errorf fmt = ksprintf (fun s -> Firebug.console##error (Js.string s)) fmt

module Request_ids : sig
  type t

  val create : unit -> t

  val set_once_exn
    :  t
    -> animation_frame_id:Dom_html.animation_frame_request_id
    -> set_timeout_id:Dom_html.timeout_id
    -> unit

  val cancelled : t -> bool
  val cancel : t -> unit
end = struct
  type ids =
    | Empty
    | Cancelled
    | Ids of
        { animation_frame_id : Dom_html.animation_frame_request_id
        ; set_timeout_id : Dom_html.timeout_id
        }

  type t = ids ref

  let create () : t = ref Empty

  let set_once_exn (t : t) ~animation_frame_id ~set_timeout_id =
    match !t with
    | Cancelled ->
      (* This should not happen, but let's be defensive. *)
      Dom_html.window##cancelAnimationFrame animation_frame_id;
      Dom_html.window##clearTimeout set_timeout_id
    | Empty -> t := Ids { animation_frame_id; set_timeout_id }
    | Ids _ -> invalid_arg "request_ids already set"
  ;;

  let cancelled x =
    match !x with
    | Cancelled -> true
    | Empty | Ids _ -> false
  ;;

  let cancel (t : t) =
    match !t with
    | Cancelled -> ()
    | Empty -> t := Cancelled
    | Ids { animation_frame_id; set_timeout_id } ->
      Dom_html.window##cancelAnimationFrame animation_frame_id;
      Dom_html.window##clearTimeout set_timeout_id;
      t := Cancelled
  ;;
end

(** [request_animation_frame] notifies the browser that you would like to do some
    computation before the next repaint. Because this needs to occur in the same
    synchronous call (called before the next repaint), returning a Deferred.t will not
    work. Instead, you pass in a job to be run before the repaint.

    Note that if [callback] contains any asynchronous work before doing DOM changes, those
    changes will not be included in the repaint and will be saved until the following one.

    When the tab is in the background, the browsers native requestAnimationFrame function
    will never call the callback, so in order to continue processing events, we set an
    alternate setTimeout at 1 second.
*)
let request_animation_frame callback =
  (* We capture the current context to use it later when handling callbacks from
     requestAnimationFrame, since exceptions raised to that would otherwise not go through
     our ordinary Async monitor. *)
  let current_context = Async_kernel_scheduler.current_execution_context () in
  let request_ids = Request_ids.create () in
  let callback () =
    if Request_ids.cancelled request_ids
    then ()
    else (
      Request_ids.cancel request_ids;
      let callback_result =
        Async_kernel_scheduler.within_context current_context callback
      in
      ignore (callback_result : (unit, unit) Result.t))
  in
  let animation_frame_id =
    let animation_callback = Js.wrap_callback (fun _ -> callback ()) in
    Dom_html.window##requestAnimationFrame animation_callback
  in
  let set_timeout_id =
    let timeout_callback = Js.wrap_callback (fun _ -> callback ()) in
    (* 1000 ms = 1s;  Chosen because backgrounded tangle sends requests
       at approximately this rate. *)
    let timeout = Js.float 1000.0 in
    Dom_html.window##setTimeout timeout_callback timeout
  in
  Request_ids.set_once_exn request_ids ~animation_frame_id ~set_timeout_id
;;

(** The Js_of_ocaml type Dom_html.element doesn't have the correct options for
    their `focus` method. Cast to this in order to work around this bug.  *)
type focusable =
  < focus : < preventScroll : bool Js.t Js.readonly_prop > Js.t -> unit Js.meth >

let as_focusable : Dom_html.element Js.t -> focusable Js.t = Js.Unsafe.coerce

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

module Logging_filter = struct
  module String_blang = struct
    module T = struct
      type t = string Blang.t [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  type t =
    | All
    | None
    | Named_filter_blang of String_blang.t
    | Custom_filter of (Sexp.t -> bool)
end

module Debug_flags : sig
  type t =
    { logging_filter : unit -> Logging_filter.t
    ; should_profile : unit -> bool
    ; should_debug : unit -> bool
    }

  val init_app
    :  app_id:string
    -> filter_names:String.Set.t
    -> debug:bool
    -> stop:unit Deferred.t
    -> t
end = struct
  type t =
    { logging_filter : unit -> Logging_filter.t
    ; should_profile : unit -> bool
    ; should_debug : unit -> bool
    }

  module App_state = struct
    type t =
      { filter_names : String.Set.t
      ; logging_filter : Logging_filter.t ref
      ; should_profile : bool ref
      ; should_debug : bool ref
      }

    let set_logging_filter t ~logging_filter = t.logging_filter := logging_filter
    let set_should_profile t ~should_profile = t.should_profile := should_profile
    let set_should_debug t ~should_debug = t.should_debug := should_debug
  end

  class type global = object
    method startLoggingAll :
      (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

    method startLogging :
      (Js.js_string Js.t -> Js.js_string Js.t Js.opt -> unit) Js.callback
        Js.writeonly_prop

    method startLoggingCustom :
      ((Js.js_string Js.t -> bool Js.t) -> Js.js_string Js.t Js.opt -> unit) Js.callback
        Js.writeonly_prop

    method stopLogging : (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

    method startProfiling :
      (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

    method stopProfiling :
      (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

    method startDebugging :
      (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

    method stopDebugging :
      (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

    method saveIncrementalGraph : (unit -> unit) Js.callback Js.writeonly_prop
  end

  let global : global Js.t = Js.Unsafe.global
  let global_is_initialized = ref false
  let app_states : App_state.t String.Table.t = String.Table.create ()

  let single_line_string_list strings =
    strings |> List.map ~f:(fun str -> "\"" ^ str ^ "\"") |> String.concat ~sep:", "
  ;;

  let multi_line_string_list strings =
    strings |> List.map ~f:(fun str -> "  " ^ str) |> String.concat ~sep:"\n"
  ;;

  let init_global ~app_filters () =
    let with_app_id_opt update_state app_id_opt =
      let app_id_opt = Js.Opt.to_option app_id_opt |> Option.map ~f:Js.to_string in
      match app_id_opt with
      | None -> Hashtbl.iter app_states ~f:update_state
      | Some app_id ->
        (match Hashtbl.find app_states app_id with
         | Some state -> update_state state
         | None ->
           print_errorf
             "Unable to find app with id \"%s\". Valid app ids are: %s"
             app_id
             (Hashtbl.keys app_states |> single_line_string_list))
    in
    let update_logging_filter logging_filter =
      with_app_id_opt (App_state.set_logging_filter ~logging_filter)
    in
    let update_should_profile should_profile =
      with_app_id_opt (App_state.set_should_profile ~should_profile)
    in
    let update_should_debug should_debug =
      with_app_id_opt (App_state.set_should_debug ~should_debug)
    in
    global##.startLoggingAll := Js.wrap_callback (update_logging_filter All);
    global##.startLogging
    := Js.wrap_callback (fun blang_str ->
         let blang_str = Js.to_string blang_str in
         with_app_id_opt (fun app_state ->
           let blang = Blang.t_of_sexp String.t_of_sexp (Sexp.of_string blang_str) in
           let invalid_names =
             Blang.fold blang ~init:String.Set.empty ~f:(fun invalid_names name ->
               if Set.mem app_state.filter_names name
               then invalid_names
               else Set.add invalid_names name)
           in
           if Set.is_empty invalid_names
           then
             App_state.set_logging_filter
               app_state
               ~logging_filter:(Named_filter_blang blang)
           else
             print_errorf
               "Unable to find named filter(s): %s. Valid names are:\n%s"
               (Set.to_list invalid_names |> single_line_string_list)
               (Set.to_list app_state.filter_names |> multi_line_string_list)));
    global##.startLoggingCustom
    := Js.wrap_callback (fun filter ->
         let filter action_sexp =
           action_sexp |> Sexp.to_string |> Js.string |> filter |> Js.to_bool
         in
         update_logging_filter (Custom_filter filter));
    global##.stopLogging := Js.wrap_callback (update_logging_filter None);
    global##.startProfiling := Js.wrap_callback (update_should_profile true);
    global##.stopProfiling := Js.wrap_callback (update_should_profile false);
    global##.startDebugging := Js.wrap_callback (update_should_debug true);
    global##.stopDebugging := Js.wrap_callback (update_should_debug false);
    global##.saveIncrementalGraph
    := Js.wrap_callback (fun () ->
         let filename = "current_incr_dom_dot_graph.dot" in
         Ui_incr.save_dot_to_file filename;
         let contents = In_channel.read_all filename in
         Vdom_file_download.create ~filename ~mimetype:"plain/text" ~contents
         |> Vdom_file_download.trigger);
    let group s ~f =
      Firebug.console##groupCollapsed (Js.string s);
      f ();
      Firebug.console##groupEnd
    in
    let log s = Firebug.console##log (Js.string s) in
    group "Incr_dom / Bonsai Console" ~f:(fun () ->
      group "Action Logging" ~f:(fun () ->
        log
          {|Logging prints action info to the console. It is disabled by default. To start logging, type one of the following:
startLoggingAll([app_id]) - log all actions
startLogging(filter_name [, app_id]) - filter actions using a pre-defined named filter [filter_name]
startLogging(filter_name_blang [, app_id]) - filter actions using a blang of named filters [filter_name_blang]
startLoggingCustom(filter [, app_id]) - filter actions using a custom function [filter] from a string (the action sexp) to a bool
To stop logging, type: stopLogging([app_id])|});
      group "Action Profiling" ~f:(fun () ->
        log
          {|Profiling is disabled by default.
To start profiling, type: startProfiling([app_id])
To stop profiling, type: stopProfiling([app_id])|});
      group "Debugging" ~f:(fun () ->
        log
          {|Debugging prints timing info to the console. It is disabled by default unless otherwise specified by the app.
To start debugging, type: startDebugging([app_id])
To stop debugging, type: stopDebugging([app_id])

[app_id] is equal to the id of the element that the incr-dom app is bound to. If the page only has one app or you want to apply the action to all apps, you can pass in [null] (or for single-argument functions, omit it altogether).|});
      log app_filters)
  ;;

  let init_app ~app_id ~filter_names ~debug ~stop =
    let app_init_message =
      sprintf
        {|Available logging filters for "%s":
        %s|}
        app_id
        (Set.to_list filter_names |> multi_line_string_list)
    in
    if not !global_is_initialized
    then (
      init_global ~app_filters:app_init_message ();
      global_is_initialized := true)
    else Firebug.console##log (Js.string app_init_message);
    let logging_filter = ref Logging_filter.None in
    let should_profile = ref false in
    let should_debug = ref debug in
    Hashtbl.set
      app_states
      ~key:app_id
      ~data:{ filter_names; logging_filter; should_profile; should_debug };
    upon stop (fun () -> Hashtbl.remove app_states app_id);
    { logging_filter = (fun () -> !logging_filter)
    ; should_profile = (fun () -> !should_profile)
    ; should_debug = (fun () -> !should_debug)
    }
  ;;
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
    Firebug.console##warn
      (Js.string
         "The browser is currently using `Quirks Mode`, which may cause your app to \
          misbehave. To use `Standards Mode`, make sure your HTML file starts with a \
          valid `<!DOCTYPE html>` declaration.")
;;

(* Adds the necessary attribute to the root node so that it can intercept
   keyboard events.
   https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/tabindex *)
let override_root_element ~simulate_body_focus_on_root_element root =
  let open Vdom in
  match (root : Node.t) with
  | Element element ->
    let focus_attrs =
      let should_add_focus_modifiers =
        element
        |> Node.Element.attrs
        |> Attr.Expert.contains_name "disable_tab_index"
        |> not
      in
      match should_add_focus_modifiers && simulate_body_focus_on_root_element with
      | true -> Vdom.Attr.(style (Css_gen.outline ~style:`None ()) @ tabindex 0)
      | false -> Vdom.Attr.empty
    in
    let add_new_attrs attrs =
      Vdom.Attr.(focus_attrs @ class_ For_mutating_inertness.app_root_class @ attrs)
    in
    element |> Node.Element.map_attrs ~f:add_new_attrs |> Node.Element
  | _ -> root
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
  (* [Inline_css] and [Async_js] inits are idempotent and so fine to do. *)
  Inline_css.Private.update_if_not_already_updated ();
  Async_js.init ();
  (* This is only really needed for tests, because the store of timings is global, so we
     could have old "start" timings around. *)
  Hashtbl.clear Performance_measure.started_timers;
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
       Performance_measure.timer_start ~profile ~debug:false Incr_app_creation;
       let obs =
         Incr.observe
           (App.create model ~old_model:model_from_last_display ~inject:Event.inject)
       in
       Performance_measure.timer_stop ~profile ~debug:false Incr_app_creation;
       let fetch (f : _ App_intf.Private.snapshot -> _) () =
         f (Incr.Observer.value_exn obs)
       in
       ( fetch (fun { view; _ } -> view)
       , fetch (fun { apply_action; _ } -> apply_action)
       , fetch (fun { update_visibility; _ } -> update_visibility)
       , fetch (fun { on_display; _ } -> on_display) )
     in
     Performance_measure.timer_start ~profile ~debug:false First_stabilization;
     Incr.stabilize ();
     Performance_measure.timer_stop ~profile ~debug:false First_stabilization;
     App.on_stabilize ();
     let named_logging_filters =
       ("all", Fn.const true) :: ("none", Fn.const false) :: named_logging_filters
       |> String.Table.of_alist_exn
     in
     let { Debug_flags.logging_filter; should_profile; should_debug } =
       let filter_names = Hashtbl.keys named_logging_filters |> String.Set.of_list in
       Debug_flags.init_app ~app_id:bind_to_element_with_id ~filter_names ~debug ~stop
     in
     let timer_start s =
       Performance_measure.timer_start
         s
         ~debug:(should_debug ())
         ~profile:(profile || should_profile ())
     in
     let timer_stop s =
       Performance_measure.timer_stop
         s
         ~debug:(should_debug ())
         ~profile:(profile || should_profile ())
     in
     timer_start Mount_initial_dom;
     let html = get_view () in
     let html_dom = Vdom.Node.to_dom html in
     let elem = Dom_html.getElementById_exn bind_to_element_with_id in
     let parent = Option.value_exn ~here:[%here] (Js.Opt.to_option elem##.parentNode) in
     Dom.replaceChild parent html_dom elem;
     timer_stop Mount_initial_dom;
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
     let potentially_refocus_root_element () =
       let refocus_root_element () =
         let element = !prev_elt in
         (* If the element to focus is an element, cast it into the
            more permissive "focusable" type defined at the top of
            this file, and then focus that. *)
         Dom_html.CoerceTo.element element
         |> Js.Opt.to_option
         |> Option.map ~f:as_focusable
         |> Option.iter ~f:(fun element ->
           element##focus
             (object%js
                val preventScroll = Js._true
             end))
       in
       (* [Js_of_ocaml.Dom_html.document] has the wrong type.
          It is listed as [Dom_html.element Js.Opt.t Js.t] but should be either

          - [Dom_html.element Js.Optdef.t Js.t] or
          - [Dom_html.element Js.Opt.t Js.Optdef.t Js.t].

          Fortunately, we can "correct" the type by promoting it
          into an [Optdef.t] and then immediately casting back down to
          an option.

          This sequence of calls produces this javascript code:
          {v
            var focus_lost;
            var active;

            var documentActiveElement = Dom_html.document##.activeElement;
            if (documentActiveElement !== undefined) {
                active = documentActiveElement;
            } else {
                active = null;
            }

            if (active === null) {
                focus_lost = true;
            } else {
                focus_lost = active.tagName == "body";
            }

            if (focus_lost && document.hasFocus()) {
              refocus_root_element()
            }
          v} *)
       let active =
         Js.Optdef.get
           (Js.Optdef.return Dom_html.document##.activeElement)
           (fun () -> Js.Opt.empty)
       in
       let focus_lost =
         Js.Opt.case
           active
           (* refocus if there is no active element. This never seems to happen in Chrome
              as of v124, but might be the case in other browsers. *)
             (fun () -> true)
           (* refocus if the active element is <body> *)
             (fun active_elt -> Js.Opt.test (Dom_html.CoerceTo.body active_elt))
       in
       (* If we are in an iframe, we don't want to steal focus unless we have focus. *)
       let has_focus =
         let document : < hasFocus : bool Js.t Js.meth > Js.t =
           Js_of_ocaml.Js.Unsafe.coerce Dom_html.document
         in
         Js_of_ocaml.Js.to_bool document##hasFocus
       in
       if focus_lost && has_focus then refocus_root_element ()
     in
     let potentially_refocus_root_element =
       if simulate_body_focus_on_root_element
       then potentially_refocus_root_element
       else fun () -> ()
     in
     if simulate_body_focus_on_root_element
     then
       (* If a [blur] event results in focus moving to [<body />], we move it back to the
          app root.

          As of Chrome v124, [blur] runs both when the [activeElement] changes,
          and when focus moves to another tab / window:
          - In the former case, a [relatedTarget] of [null] means that focus will move to
            [<body />].
          - In the latter case, [blur] is dispatched twice; with [relatedTarget] being
            [null] and [undefined] respectively, but [activeElement] doesn't change.

          That's why it's critical that in [potentially_refocus_root_element], we check
          that [activeElement] is on the [<body />] or [null],
          AND that [document.hasFocus()] returns [true].

          We run this on [capture] because [blur] doesn't bubble.
       *)
       ignore
       @@ Dom.addEventListenerWithOptions
            Dom_html.window
            Dom_html.Event.blur
            ~capture:Js._true
            (Dom_html.handler (fun e ->
               (* [Js.Unsafe.*] is like [Obj.magic]. We should be explicit about what we
                  expect. *)
               let e
                 : < relatedTarget : Dom_html.element Js.t Js.opt Js.readonly_prop > Js.t
                 =
                 Js.Unsafe.coerce e
               in
               let receiving_focus = e##.relatedTarget in
               if not (Js.Opt.test receiving_focus)
               then potentially_refocus_root_element ();
               Js._true));
     let update_visibility () =
       Visibility.mark_clean visibility;
       let new_model =
         (get_update_visibility ())
           ~schedule_event:Ui_effect.Expert.handle
           (Incr.Var.latest_value model_v)
       in
       Incr.Var.set model_v new_model;
       timer_start Stabilize_for_update_visibility;
       Incr.stabilize ();
       timer_stop Stabilize_for_update_visibility;
       App.on_stabilize ()
     in
     let maybe_log_action =
       let safe_filter ~name filter action =
         match Or_error.try_with (fun () -> filter action) with
         | Ok should_log -> should_log
         | Error err ->
           print_errorf !"Exception raised by %s: %{Error#hum}" name err;
           false
       in
       let named_filter_blang_cache =
         Core.Memo.of_comparable
           (module Logging_filter.String_blang)
           (fun blang ->
             let filter = Hashtbl.find_exn named_logging_filters in
             safe_filter
               ~name:(sprintf !"named filter blang \"%{sexp:string Blang.t}\"" blang)
               (match blang with
                | Base name -> filter name
                | _ -> fun action -> Blang.eval blang (fun name -> filter name action)))
       in
       fun action ->
         let should_log_action =
           match logging_filter () with
           | All -> true
           | None -> false
           | Named_filter_blang blang -> named_filter_blang_cache blang action
           | Custom_filter filter ->
             safe_filter
               ~name:"custom filter"
               (fun action -> filter (App.Action.sexp_of_t action))
               action
         in
         if should_log_action
         then Async_js.log_s_as_string [%message "Action" (action : App.Action.t)]
     in
     (* It's important that we don't call [Incr.Var.set] within [apply_action] unless
        we're also going to stabilize. Some code in Bonsai relies on this assumption
        as part of its [action_requires_stabilization] logic. Breaking this invariant
        won't break Bonsai code, but it will effectively remove an optimization. *)
     let apply_action action model =
       maybe_log_action action;
       if App.action_requires_stabilization action
       then (
         Incr.Var.set model_v model;
         timer_start Stabilize_for_action;
         Incr.stabilize ();
         timer_stop Stabilize_for_action;
         App.on_stabilize ())
       else (
         Bonsai_metrics.Counters.observe Incr_skipped_stabilizations;
         if should_debug ()
         then Firebug.console##debug (Js.string "action applied without stabilizing"));
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
     let perform_update () =
       timer_stop Start_of_frame_to_start_of_next_frame;
       timer_start Start_of_frame_to_start_of_next_frame;
       timer_stop End_of_frame_to_start_of_next_frame;
       timer_start Whole_animation_frame_loop;
       timer_start Stabilize_for_clock;
       (* The clock is set only once per call to perform_update, so that all actions that
          occur before each display update occur "at the same time." *)
       let now =
         let date = new%js Js.date_now in
         Time_ns.Span.of_ms (Js.to_float date##getTime) |> Time_ns.of_span_since_epoch
       in
       Incr.Clock.advance_clock Incr.clock ~to_:now;
       App.advance_clock_to now;
       Incr.stabilize ();
       timer_stop Stabilize_for_clock;
       App.on_stabilize ();
       timer_start Update_visibility;
       if Visibility.is_dirty visibility then update_visibility ();
       timer_stop Update_visibility;
       timer_start Apply_actions;
       apply_actions (Incr.Var.value model_v);
       timer_stop Apply_actions;
       timer_start Stabilize_after_all_apply_actions;
       Incr.stabilize ();
       timer_stop Stabilize_after_all_apply_actions;
       App.on_stabilize ();
       let html =
         get_view () |> override_root_element ~simulate_body_focus_on_root_element
       in
       timer_start Diff_vdom;
       let patch = Vdom.Node.Patch.create ~previous:!prev_html ~current:html in
       timer_stop Diff_vdom;
       if not (Vdom.Node.Patch.is_empty patch) then Visibility.mark_dirty visibility;
       timer_start Patch_vdom;
       let elt = Vdom.Node.Patch.apply patch !prev_elt in
       timer_stop Patch_vdom;
       timer_start On_display_handlers;
       (get_on_display ()) state ~schedule_event:Ui_effect.Expert.handle;
       timer_stop On_display_handlers;
       Incr.Var.set model_from_last_display_v (Incr.Var.value model_v);
       prev_html := html;
       prev_elt := elt;
       if should_debug () then Firebug.console##debug (Js.string "-------");
       (* Restoring focus from the [<body />] to the app root should mostly be handled by
          the [blur] listener above, but we additionally run this check every frame because:

          - We want the root element to start out focused, so perform an initial
            update/render, then immediately focus the root (unless a non-body element
            already has focus).
          - [blur] doesn't run if the currently focused element is removed from the DOM,
            so we might need to possibly focus-steal after every frame.

          We keep the check on [blur], so that we can respond immediately to most blurs
          without waiting ~16ms for the next frame.
       *)
       potentially_refocus_root_element ();
       timer_stop Whole_animation_frame_loop;
       timer_start End_of_frame_to_start_of_next_frame
     in
     (* We use [request_animation_frame] so that browser tells us where it's time to
        refresh the UI. All the actions will be processed and the changes propagated
        to the DOM in one frame. *)
     let rec callback () =
       if Deferred.is_determined stop
       then ()
       else (
         perform_update ();
         request_animation_frame callback)
     in
     perform_update ();
     request_animation_frame callback;
     Deferred.never ());
  (*
     If the app has enabled profiling, we clear marks / measures occasionally to avoid
     a memory leak.

     Note that {!Performance.clear_marks} and {!Performance.clear_measures} will only
     clear from the performance buffer. This will not affect the Performance
     Timeline in the DevTools panel (if in use), or apps pulling new data from
     a [PerformanceObserver]. *)
  if profile
  then
    Clock_ns.every (Time_ns.Span.of_int_sec 2) (fun () ->
      Performance_measure.clear_associated_marks_and_measures ())
;;

module Private = struct
  let start_bonsai = start_bonsai
  let time_source = time_source
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
