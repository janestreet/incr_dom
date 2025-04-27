open! Core
open Js_of_ocaml

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
    alternate setTimeout at 1 second. *)
let request_animation_frame callback =
  (* We capture the current context to use it later when handling callbacks from
     requestAnimationFrame, since exceptions raised to that would otherwise not go through
     our ordinary Async monitor. *)
  let current_context =
    Async_kernel.Async_kernel_scheduler.current_execution_context ()
  in
  let request_ids = Request_ids.create () in
  let callback () =
    if Request_ids.cancelled request_ids
    then ()
    else (
      Request_ids.cancel request_ids;
      let callback_result =
        Async_kernel.Async_kernel_scheduler.within_context current_context callback
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

let start_looping ~is_stopped ~perform_update =
  (* We use [request_animation_frame] so that browser tells us where it's time to
     refresh the UI. All the actions will be processed and the changes propagated
     to the DOM in one frame. *)
  let rec callback () =
    if is_stopped ()
    then ()
    else (
      perform_update ();
      request_animation_frame callback)
  in
  perform_update ();
  request_animation_frame callback
;;
