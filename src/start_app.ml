open! Core_kernel
open Virtual_dom
open Async_kernel
open Js_of_ocaml

let timer_start s ~debug = if debug then Firebug.console##time (Js.string s)

let timer_stop s ~debug = if debug then Firebug.console##timeEnd (Js.string s)

(** [request_animation_frame] notifies the browser that you would like to do some
    computation before the next repaint. Because this needs to occur in the same
    synchronous call (called before the next repaint), returning a Deferred.t will not
    work. Instead, you pass in a job to be run before the repaint.

    Note that if [callback] contains any asynchronous work before doing DOM changes, those
    changes will not be included in the repaint and will be saved until the following one.
*)
let request_animation_frame callback =
  (* We capture the current context to use it later when handling callbacks from
     requestAnimationFrame, since exceptions raised to that would otherwise not go through
     our ordinary Async monitor. *)
  let current_context = Async_kernel_scheduler.(current_execution_context (t ())) in
  ignore
    ( Dom_html.window##requestAnimationFrame
        (Js.wrap_callback (fun _timestamp ->
           ignore
             ( Async_kernel_scheduler.within_context current_context callback
               : (unit, unit) Result.t )))
      : Dom_html.animation_frame_request_id )
;;

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

  (** returns a deferred that becomes determined next time we're dirty, so immediately if
      it's already dirty.  *)
  val when_dirty : t -> unit Deferred.t
end = struct
  type t = { mutable when_dirty : unit Ivar.t }

  let create_as_dirty () = { when_dirty = Ivar.create_full () }

  let mark_dirty t = Ivar.fill_if_empty t.when_dirty ()

  let is_dirty t = Ivar.is_full t.when_dirty

  let when_dirty t = Ivar.read t.when_dirty

  let mark_clean t = if is_dirty t then t.when_dirty <- Ivar.create ()
end

module Action_log : sig
  val init : unit -> unit

  val should_log : unit -> bool
end = struct
  class type global =
    object
      method logFlag : bool Js.t Js.writeonly_prop
      method logFlag_untyped : 'a Js.t Js.optdef Js.readonly_prop
      method startLogging : (unit -> unit) Js.callback Js.writeonly_prop
      method stopLogging : (unit -> unit) Js.callback Js.writeonly_prop
    end

  let global : global Js.t = Js.Unsafe.global

  let init () =
    let set_flag b = global##.logFlag := Js.bool b in
    set_flag false;
    global##.startLogging := Js.wrap_callback (fun () -> set_flag true);
    global##.stopLogging := Js.wrap_callback (fun () -> set_flag false);
    let init_message =
      " Incr_dom action logging is disabled by default.\n \
       To start logging actions, type startLogging()\n \
       To stop logging actions, type stopLogging()\n"
    in
    Firebug.console##log (Js.string init_message)
  ;;

  let should_log () =
    Js.Optdef.case global##.logFlag_untyped (Fn.const false) (fun log_flag ->
      match Js.to_string (Js.typeof log_flag) with
      | "boolean" -> Js.to_bool log_flag
      | _ -> false)
  ;;
end

let derived
      (type model)
      ?bind_to_element_with_id
      ?(debug=false)
      ?(stop = Deferred.never ())
      ~initial_model
      (module App : App_intf.S_derived with type Model.t = model)
  =
  (* This is idempotent and so fine to do. *)
  Async_js.init ();
  don't_wait_for
    (let%bind () = Async_js.document_loaded () in
     let model_v = Incr.Var.create initial_model in
     let model = Incr.Var.watch model_v in
     Incr.set_cutoff
       model
       (Incr.Cutoff.create (fun ~old_value ~new_value ->
          App.Model.cutoff old_value new_value));
     let derived_model_incr = App.Derived_model.create model in
     let r, w = Pipe.create () in
     let schedule_action action = Pipe.write_without_pushback w action in
     let module Event = Vdom.Event.Define (struct
                          module Action = App.Action

                          let handle action = Pipe.write_without_pushback w action
                        end)
     in
     let visibility = Visibility.create_as_dirty () in
     let viewport_changed () = Visibility.mark_dirty visibility in
     (* This registers the [viewport_changed] handler with Virtual_dom. If event handlers
        use the [Vdom.Event.Viewport_changed] event, we are notified. *)
     let module Viewport_handler = Vdom.Event.Define_visibility (struct
                                     let handle = viewport_changed
                                   end)
     in
     let view = Incr.observe (App.view model derived_model_incr ~inject:Event.inject) in
     let derived_model = Incr.observe derived_model_incr in
     let module Models = struct
       type t =
         { model : App.Model.t
         ; derived_model : App.Derived_model.t
         }
     end
     in
     let get_current_models () =
       { Models.model = Incr.Var.value model_v
       ; derived_model = Incr.Observer.value_exn derived_model
       }
     in
     Incr.stabilize ();
     let html = Incr.Observer.value_exn view in
     let html_dom = Vdom.Node.to_dom html in
     let elt = (html_dom :> Dom.element Js.t) in
     let models = get_current_models () in
     (match bind_to_element_with_id with
      | None -> Dom_html.document##.body := html_dom
      | Some id ->
        let elem = Dom_html.getElementById_exn id in
        let parent =
          Option.value_exn ~here:[%here] (Js.Opt.to_option elem##.parentNode)
        in
        Dom.replaceChild parent elt elem);
     (* we make sure to call [viewport_changed] whenever the window resizes or the scroll
        container in which our HTML is located is scrolled. *)
     let call_viewport_changed_on_event event_name where =
       ignore
         ( Dom.addEventListener
             where
             (Dom.Event.make event_name)
             (Dom.handler (fun _ ->
                viewport_changed ();
                Js._true))
             Js._false
           : Dom.event_listener_id )
     in
     call_viewport_changed_on_event "scroll" (Js_misc.get_scroll_container html_dom);
     call_viewport_changed_on_event "resize" Dom_html.window;
     let%bind state =
       App.on_startup
         ~schedule_action
         (Incr.Var.value model_v)
         (Incr.Observer.value_exn derived_model)
     in
     let prev_models = ref models in
     let prev_html = ref html in
     let prev_elt = ref elt in
     let recompute_derived model =
       Incr.Var.set model_v model;
       Incr.stabilize ();
       Incr.Observer.value_exn derived_model
     in
     let update_visibility () =
       Visibility.mark_clean visibility;
       Incr.stabilize ();
       let new_model =
         App.update_visibility
           ~recompute_derived
           (Incr.Var.value model_v)
           (Incr.Observer.value_exn derived_model)
       in
       Incr.Var.set model_v new_model
     in
     let apply_action action =
       if App.Action.should_log action
       then Async_js.Debug.log_s [%message "Action" (action : App.Action.t)];
       let old_model = Incr.Var.value model_v in
       let new_model =
         App.apply_action action old_model state ~schedule_action ~recompute_derived
       in
       Incr.Var.set model_v new_model
     in
     let rec apply_actions pipe =
       match Pipe.read_now pipe with
       | `Eof -> failwith "bug: Action pipe closed"
       | `Nothing_available -> ()
       | `Ok action ->
         apply_action action;
         apply_actions pipe
     in
     let perform_update pipe =
       timer_start "total" ~debug;
       timer_start "update visibility" ~debug;
       if Visibility.is_dirty visibility then update_visibility ();
       timer_stop "update visibility" ~debug;
       timer_start "apply actions" ~debug;
       apply_actions pipe;
       timer_stop "apply actions" ~debug;
       timer_start "stabilize" ~debug;
       let now =
         let date = new%js Js.date_now in
         Time_ns.Span.of_ms date##getTime |> Time_ns.of_span_since_epoch
       in
       Incr.advance_clock ~to_:now;
       Incr.stabilize ();
       timer_stop "stabilize" ~debug;
       (* Compute the current models immediately after stabilization, for use on the next
          (not the current) iteration, because now we are sure to have the model and the
          derived model in sync.*)
       let models = get_current_models () in
       let html = Incr.Observer.value_exn view in
       timer_start "diff" ~debug;
       let patch = Vdom.Node.Patch.create ~previous:!prev_html ~current:html in
       timer_stop "diff" ~debug;
       if not (Vdom.Node.Patch.is_empty patch) then Visibility.mark_dirty visibility;
       timer_start "patch" ~debug;
       let elt = Vdom.Node.Patch.apply patch !prev_elt in
       timer_stop "patch" ~debug;
       timer_start "on_display" ~debug;
       App.on_display (* Retrieve the immutable_summary from the previous iteration *)
         ~old_model:!prev_models.model
         ~old_derived_model:!prev_models.derived_model
         (Incr.Var.value model_v)
         (Incr.Observer.value_exn derived_model)
         state
         ~schedule_action;
       timer_stop "on_display" ~debug;
       prev_models := models;
       prev_html := html;
       prev_elt := elt;
       timer_stop "total" ~debug;
       if debug then Firebug.console##debug (Js.string "-------")
     in
     (* We use [request_animation_frame] so that browser tells us where it's time to
        refresh the UI. All the actions will be processed and the changes propagated
        to the DOM in one frame. *)
     let rec callback () =
       if Deferred.is_determined stop
       then ()
       else if not (Visibility.is_dirty visibility) && Pipe.is_empty r
       then
         don't_wait_for
           (* Wait until actions have been enqueued before scheduling an animation frame *)
           (let%map () =
              Deferred.any_unit
                [ Deferred.ignore (Pipe.values_available r : [`Eof | `Ok] Deferred.t)
                ; Visibility.when_dirty visibility
                ]
            in
            request_animation_frame callback)
       else (
         perform_update r;
         request_animation_frame callback)
     in
     request_animation_frame callback;
     Deferred.never ())
;;

let component
      (type model)
      ?bind_to_element_with_id
      ?(debug=false)
      ?(stop = Deferred.never ())
      ~initial_model
      (module App : App_intf.S_component with type Model.t = model)
  =
  (* This is idempotent and so fine to do. *)
  Async_js.init ();
  don't_wait_for
    (let%bind () = Async_js.document_loaded () in
     let model_v = Incr.Var.create initial_model in
     let model = Incr.Var.watch model_v in
     let model_from_last_display_v = Incr.Var.create initial_model in
     let model_from_last_display = Incr.Var.watch model_from_last_display_v in
     Incr.set_cutoff
       model
       (Incr.Cutoff.create (fun ~old_value ~new_value ->
          App.Model.cutoff old_value new_value));
     let r, w = Pipe.create () in
     let schedule_action action = Pipe.write_without_pushback w action in
     let module Event = Vdom.Event.Define (struct
                          module Action = App.Action

                          let handle action = Pipe.write_without_pushback w action
                        end)
     in
     let visibility = Visibility.create_as_dirty () in
     let viewport_changed () = Visibility.mark_dirty visibility in
     (* This registers the [viewport_changed] handler with Virtual_dom. If event handlers
        use the [Vdom.Event.Viewport_changed] event, we are notified. *)
     let module Viewport_handler = Vdom.Event.Define_visibility (struct
                                     let handle = viewport_changed
                                   end)
     in
     let app =
       Incr.observe
         (App.create model ~old_model:model_from_last_display ~inject:Event.inject)
     in
     Incr.stabilize ();
     Action_log.init ();
     let html = Incr.Observer.value_exn app |> Component.view in
     let html_dom = Vdom.Node.to_dom html in
     let elt = (html_dom :> Dom.element Js.t) in
     (match bind_to_element_with_id with
      | None -> Dom_html.document##.body := html_dom
      | Some id ->
        let elem = Dom_html.getElementById_exn id in
        let parent =
          Option.value_exn ~here:[%here] (Js.Opt.to_option elem##.parentNode)
        in
        Dom.replaceChild parent elt elem);
     (* we make sure to call [viewport_changed] whenever the window resizes or the scroll
        container in which our HTML is located is scrolled. *)
     let call_viewport_changed_on_event event_name where =
       ignore
         ( Dom.addEventListener
             where
             (Dom.Event.make event_name)
             (Dom.handler (fun _ ->
                viewport_changed ();
                Js._true))
             Js._false
           : Dom.event_listener_id )
     in
     call_viewport_changed_on_event "scroll" (Js_misc.get_scroll_container html_dom);
     call_viewport_changed_on_event "resize" Dom_html.window;
     let%bind state = App.on_startup ~schedule_action (Incr.Var.value model_v) in
     let prev_html = ref html in
     let prev_elt = ref elt in
     let update_visibility () =
       Visibility.mark_clean visibility;
       let new_model = Component.update_visibility (Incr.Observer.value_exn app) () in
       Incr.Var.set model_v new_model;
       timer_start "stabilize" ~debug;
       Incr.stabilize ();
       timer_stop "stabilize" ~debug
     in
     let apply_action action =
       if Action_log.should_log ()
       then Async_js.Debug.log_s [%message "Action" (action : App.Action.t)];
       let new_model =
         (app |> Incr.Observer.value_exn |> Component.apply_action)
           action
           state
           ~schedule_action
       in
       Incr.Var.set model_v new_model;
       timer_start "stabilize" ~debug;
       Incr.stabilize ();
       timer_stop "stabilize" ~debug
     in
     let rec apply_actions pipe =
       match Pipe.read_now pipe with
       | `Eof -> failwith "bug: Action pipe closed"
       | `Nothing_available -> ()
       | `Ok action ->
         apply_action action;
         apply_actions pipe
     in
     let perform_update pipe =
       timer_start "stabilize" ~debug;
       (* The clock is set only once per call to perform_update, so that all actions that
          occur before each display update occur "at the same time." *)
       let now =
         let date = new%js Js.date_now in
         Time_ns.Span.of_ms date##getTime |> Time_ns.of_span_since_epoch
       in
       Incr.advance_clock ~to_:now;
       Incr.stabilize ();
       timer_stop "stabilize" ~debug;
       timer_start "total" ~debug;
       timer_start "update visibility" ~debug;
       if Visibility.is_dirty visibility then update_visibility ();
       timer_stop "update visibility" ~debug;
       timer_start "apply actions" ~debug;
       apply_actions pipe;
       timer_stop "apply actions" ~debug;
       let html = Incr.Observer.value_exn app |> Component.view in
       timer_start "diff" ~debug;
       let patch = Vdom.Node.Patch.create ~previous:!prev_html ~current:html in
       timer_stop "diff" ~debug;
       if not (Vdom.Node.Patch.is_empty patch) then Visibility.mark_dirty visibility;
       timer_start "patch" ~debug;
       let elt = Vdom.Node.Patch.apply patch !prev_elt in
       timer_stop "patch" ~debug;
       timer_start "on_display" ~debug;
       Component.on_display (Incr.Observer.value_exn app) state ~schedule_action;
       timer_stop "on_display" ~debug;
       Incr.Var.set model_from_last_display_v (Incr.Var.value model_v);
       prev_html := html;
       prev_elt := elt;
       timer_stop "total" ~debug;
       if debug then Firebug.console##debug (Js.string "-------")
     in
     (* We use [request_animation_frame] so that browser tells us where it's time to
        refresh the UI. All the actions will be processed and the changes propagated
        to the DOM in one frame. *)
     let rec callback () =
       if Deferred.is_determined stop
       then ()
       else if not (Visibility.is_dirty visibility) && Pipe.is_empty r
       then
         don't_wait_for
           (* Wait until actions have been enqueued before scheduling an animation frame *)
           (let%map () =
              Deferred.any_unit
                [ Deferred.ignore (Pipe.values_available r : [`Eof | `Ok] Deferred.t)
                ; Visibility.when_dirty visibility
                ]
            in
            request_animation_frame callback)
       else (
         perform_update r;
         request_animation_frame callback)
     in
     request_animation_frame callback;
     Deferred.never ())
;;

(** Trivially lift the simple App_intf into a derived one *)
module Make_simple_derived (App : App_intf.S_simple) :
  App_intf.S_derived with type Model.t = App.Model.t and type Action.t = App.Action.t =
struct
  module Model = App.Model
  module State = App.State

  module Derived_model = struct
    type t = unit

    let create (_ : Model.t Incr.t) = Incr.const ()
  end

  module Action = struct
    include App.Action
  end

  let apply_action
        t
        model
        state
        ~schedule_action
        ~recompute_derived:(_ : Model.t -> Derived_model.t)
    =
    App.apply_action t model state ~schedule_action
  ;;

  let update_visibility model () ~recompute_derived:_ = App.update_visibility model

  let on_startup ~schedule_action model () = App.on_startup ~schedule_action model

  let view model (_ : unit Incr.t) ~inject = App.view model ~inject

  let on_display ~old_model ~old_derived_model:_ model () state =
    App.on_display ~old_model model state
  ;;
end

let simple
      (type model)
      ?bind_to_element_with_id
      ?debug
      ?stop
      ~initial_model
      (module App : App_intf.S_simple with type Model.t = model)
  =
  derived
    ?bind_to_element_with_id
    ?debug
    ?stop
    ~initial_model
    (module Make_simple_derived (App))
;;
