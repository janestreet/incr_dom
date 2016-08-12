open! Core_kernel.Std
open Virtual_dom.Std
open Async_kernel.Std
open Js_of_ocaml
open Js_of_ocaml_async
open Common

let document_loaded : unit Deferred.t =
  let ready_state = Js.to_string Dom_html.document##.readyState in
  if String.(ready_state = "complete" || ready_state = "loaded")
  then Deferred.unit
  else (
    let loaded = Ivar.create () in
    ignore (
      Dom_html.addEventListener
        Dom_html.document
        (Dom.Event.make "DOMContentLoaded")
        (Dom.handler (fun _ -> Ivar.fill loaded (); Js._true))
        Js._false);
    Ivar.read loaded
  )

let timer_start s =
  Firebug.console##time (Js.string s)
;;

let timer_stop s =
  Firebug.console##timeEnd (Js.string s)
;;

(** [request_animation_frame] notifies the browser that you would like to do some
    computation before the next repaint. Because this needs to occur in the same
    synchronous call (called before the next repaint), returning a Deferred.t will not
    work. Instead, you pass in a job to be run before the repaint.

    Note that if [callback] contains any asynchronous work before doing DOM changes, those
    changes will not be included in the repaint and will be saved until the following one.
*)
let request_animation_frame callback =
  ignore (
    Dom_html.window##requestAnimationFrame
      (Js.wrap_callback (fun _timestamp -> callback ()))
    : Dom_html.animation_frame_request_id)
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
  type t = { mutable when_dirty: unit Ivar.t }

  let create_as_dirty () =
    { when_dirty  = Ivar.create_full () }

  let mark_dirty t = Ivar.fill_if_empty t.when_dirty ()
  let is_dirty t = Ivar.is_full t.when_dirty
  let when_dirty t = Ivar.read t.when_dirty

  let mark_clean t =
    if is_dirty t then (
      t.when_dirty <- Ivar.create ()
    )
end


let start_derived
      (type model) (type derived) (type action)
      ?bind_to_element_with_id
      ~initial_state
      ~on_startup
      ~project_immutable_summary
      ~on_display
      (module App : App_intf.S_derived
        with type Model.t = model
         and type Action.t = action
         and type Derived_model.t = derived)
  =
  (* This is idempotent and so fine to do. *)
  Async_js.init ();
  don't_wait_for (
    document_loaded >>= fun () ->
    let state = Incr.Var.create initial_state in
    let derived_state_incr = App.Derived_model.create (Incr.Var.watch state) in

    let (r, w) = Pipe.create () in
    let schedule action = Pipe.write_without_pushback w action in

    let visibility = Visibility.create_as_dirty () in
    let viewport_changed () = Visibility.mark_dirty visibility in

    let view =
      Incr.observe
        (App.view
           (Incr.Var.watch state)
           derived_state_incr
           ~schedule
           ~viewport_changed)
    in
    let derived_state = Incr.observe derived_state_incr in
    let get_derived_state () = Incr.stabilize (); Incr.Observer.value_exn derived_state in
    let extract_immutable_summary () =
      project_immutable_summary
        (Incr.Var.value state)
        (Incr.Observer.value_exn derived_state)
    in

    Incr.stabilize ();

    let html = Incr.Observer.value_exn view in
    let html_dom = Vdom.Node.to_dom html in
    let elt = (html_dom :> Dom.element Js.t) in
    let immutable_summary = extract_immutable_summary () in

    (match bind_to_element_with_id with
     | None ->
       Dom_html.document##.body := html_dom;
     | Some id ->
       let elem = Dom_html.getElementById id in
       let parent =
         Option.value_exn ~here:[%here] (Js.Opt.to_option elem##.parentNode)
       in
       Dom.replaceChild parent elt elem
    );

    (* we make sure to call [viewport_changed] whenever the window resizes or the scroll
       container in which our HTML is located is scrolled. *)
    let call_viewport_changed_on_event event_name where =
      ignore (
        Dom.addEventListener
          where
          (Dom.Event.make event_name)
          (Dom.handler (fun _ -> viewport_changed (); Js._true))
          Js._false
        : Dom.event_listener_id)
    in
    call_viewport_changed_on_event "scroll"
      (Js_misc.get_scroll_container (html_dom :> Dom.node Js.t));
    call_viewport_changed_on_event "resize" Dom_html.window;

    on_startup ~schedule state;

    let prev_immutable_summary = ref immutable_summary in
    let prev_html = ref html in
    let prev_elt = ref elt in

    let update_visibility () =
      Visibility.mark_clean visibility;
      Incr.stabilize ();
      let new_state =
        App.update_visibility
          (Incr.Var.value state)
          (Incr.Observer.value_exn derived_state)
      in
      Incr.Var.set state new_state
    in

    let apply_action action =
      if App.Action.should_log action then begin
        logf !"Action: %{sexp:App.Action.t}" action
      end;
      let old_state = Incr.Var.value state in
      let new_state =
        App.Action.apply
          action
          ~schedule
          old_state
          ~stabilize_and_get_derived:get_derived_state
      in
      Incr.Var.set state new_state
    in

    let rec apply_actions pipe =
      match Pipe.read_now pipe with
      | `Eof -> failwith "bug: Action pipe closed"
      | `Nothing_available -> ()
      | `Ok action -> apply_action action; apply_actions pipe
    in

    let perform_update pipe =
      timer_start "total";

      timer_start "update visibility";
      if Visibility.is_dirty visibility then (
        update_visibility ()
      );
      timer_stop "update visibility";

      timer_start "apply actions";
      apply_actions pipe;
      timer_stop "apply actions";

      timer_start "stabilize";
      let now =
        let date = new%js Js.date_now in
        Time_ns.Span.of_ms date##getTime
        |> Time_ns.of_span_since_epoch
      in
      Incr.advance_clock ~to_:now;
      Incr.stabilize ();
      timer_stop "stabilize";

      (* Compute the immutable summary of the model immediately after
         stabilization for use on the next (not the current) iteration, because
         now we are sure to have the model and the derived model in sync.*)
      let immutable_summary = extract_immutable_summary () in
      let html = Incr.Observer.value_exn view in

      timer_start "diff";
      let patch = Vdom.Node.Patch.create ~previous:!prev_html ~current:html in
      timer_stop "diff";

      if not (Vdom.Node.Patch.is_empty patch) then (
        Visibility.mark_dirty visibility;
      );

      timer_start "patch";
      let elt = Vdom.Node.Patch.apply patch !prev_elt in
      timer_stop "patch";

      timer_start "on_display";
      on_display
        ~schedule
        (* Retrieve the immutable_summary from the previous iteration *)
        ~old:!prev_immutable_summary
        (Incr.Var.value state)
        (Incr.Observer.value_exn derived_state);
      timer_stop "on_display";

      prev_immutable_summary := immutable_summary;
      prev_html := html;
      prev_elt := elt;

      timer_stop "total";
      Firebug.console##debug (Js.string "-------")
    in

    (* We use [request_animation_frame] so that browser tells us where it's time to
       refresh the UI. All the actions will be processed and the changes propagated
       to the DOM in one frame. *)
    let rec callback () =
      if not (Visibility.is_dirty visibility) && Pipe.is_empty r then (
        don't_wait_for (
          (* Wait until actions have been enqueued before scheduling an animation frame *)
          Deferred.any_unit
            [ Deferred.ignore (Pipe.values_available r : ([ `Eof | `Ok ] Deferred.t))
            ; Visibility.when_dirty visibility
            ]
          >>| fun () ->
          request_animation_frame callback
        )
      ) else (
        perform_update r;
        request_animation_frame callback
      )
    in

    request_animation_frame callback;
    Deferred.never ()
  )

(** Trivially lift a simple App_intf into a derived one. *)
module Make_derived (App : App_intf.S) :
  (App_intf.S_derived
   with type Model.t = App.Model.t
    and type Action.t = App.Action.t
    and type Derived_model.t = unit)
= struct
  module Model = App.Model
  module Derived_model = struct
    type t = unit
    let create (_ : Model.t Incr.t) = Incr.const ()
  end
  module Action = struct
    include App.Action
    let apply t ~schedule model ~stabilize_and_get_derived:(_ : unit -> Derived_model.t) =
      apply t ~schedule model
  end
  let update_visibility model (_ : unit) = App.update_visibility model
  let view model (_ : unit Incr.t) ~schedule = App.view model ~schedule
end

let start
      (type model) (type action)
      ?bind_to_element_with_id
      ~initial_state
      ~on_startup
      ~project_immutable_summary
      ~on_display
      (module App : App_intf.S with type Model.t = model and type Action.t = action)
  =
  start_derived
    ?bind_to_element_with_id
    ~initial_state
    ~on_startup
    ~project_immutable_summary:(fun model () -> project_immutable_summary model)
    ~on_display:(fun ~schedule ~old model () -> on_display ~schedule ~old model)
    (module Make_derived(App))
