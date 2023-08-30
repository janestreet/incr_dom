open! Core
module Incr = Ui_incr
module Effect = Ui_effect

type t =
  { incr : Incr.Clock.t
  ; timing_wheel : (unit, unit) Ui_effect.Private.Callback.t Timing_wheel.t
  ; mutable
      add_before_advance :
      (Time_ns.t * (unit, unit) Ui_effect.Private.Callback.t) Reversed_list.t
  ; mutable
      wait_after_display_callbacks :
      (unit, unit) Ui_effect.Private.Callback.t Reversed_list.t
  ; mutable advance_to : Time_ns.t option
  }

let invariant t =
  (* This is only a soft invariant (it prints instead of raising) because it
     probably isn't fatal if the two clocks are out of sync. We want to know
     about it if they are, though. *)
  let wheel_now = Timing_wheel.now t.timing_wheel in
  let incr_now = Incr.Clock.now t.incr in
  if not (Time_ns.equal wheel_now incr_now)
  then
    eprint_s
      [%message
        "BUG: timing wheel and incremental clock are out of sync"
          (wheel_now : Time_ns.Alternate_sexp.t)
          (incr_now : Time_ns.Alternate_sexp.t)]
;;

let create ~start =
  let start =
    (* We round the start time to match what [Incr.Clock.create] does. *)
    Time_ns.of_time_float_round_nearest_microsecond
      (Time_ns.to_time_float_round_nearest_microsecond start)
  in
  let config = Incr.Clock.default_timing_wheel_config in
  let incr = Incr.Clock.create ~timing_wheel_config:config ~start () in
  let timing_wheel = Timing_wheel.create ~config ~start in
  let t =
    { incr
    ; timing_wheel
    ; add_before_advance = []
    ; wait_after_display_callbacks = []
    ; advance_to = None
    }
  in
  invariant t;
  t
;;

let incr_clock t = t.incr

let now t =
  match t.advance_to with
  | Some to_ -> to_
  | None -> Timing_wheel.now t.timing_wheel
;;

let at_intervals t span = Incr.Clock.at_intervals t.incr span
let watch_now t = Incr.Clock.watch_now t.incr
let at t at = Incr.Clock.at t.incr at

let advance_clock t ~to_ =
  assert (Time_ns.( >= ) to_ (now t));
  t.advance_to <- Some to_
;;

let advance_clock_by t span = advance_clock t ~to_:(Time_ns.add (now t) span)

(* [until], [sleep], and [wait_after_display] all want to add alarms to [t.timing_wheel],
   which throws if we're already in the middle of an alarm. Instead of adding it to the
   timing wheel immediately, we store it and then add it next time the timing wheel is
   advanced.

   Note: the alarms are added as part of [flush], which is the only place that we actually
   advance [t.time_source], which makes this approach sound. *)
let until t at =
  Effect.Private.make ~request:() ~evaluator:(fun callback ->
    t.add_before_advance <- (at, callback) :: t.add_before_advance)
;;

let sleep t span =
  Effect.Private.make ~request:() ~evaluator:(fun callback ->
    let at = Time_ns.add (now t) span in
    t.add_before_advance <- (at, callback) :: t.add_before_advance)
;;

let wait_after_display t =
  Effect.Private.make ~request:() ~evaluator:(fun callback ->
    t.wait_after_display_callbacks <- callback :: t.wait_after_display_callbacks)
;;

module Private = struct
  let flush t =
    let handle_fired callback =
      Effect.Expert.handle
        (Effect.Private.Callback.respond_to
           (Timing_wheel.Alarm.value t.timing_wheel callback)
           ())
    in
    List.iter (Reversed_list.rev t.add_before_advance) ~f:(fun (at, callback) ->
      let (_ : _ Timing_wheel.Alarm.t) = Timing_wheel.add t.timing_wheel ~at callback in
      ());
    t.add_before_advance <- [];
    (match t.advance_to with
     | Some to_ ->
       t.advance_to <- None;
       Timing_wheel.advance_clock t.timing_wheel ~to_ ~handle_fired;
       Timing_wheel.fire_past_alarms t.timing_wheel ~handle_fired;
       Incr.Clock.advance_clock t.incr ~to_
     | None -> Timing_wheel.fire_past_alarms t.timing_wheel ~handle_fired);
    invariant t
  ;;

  let trigger_after_display t =
    let callbacks = t.wait_after_display_callbacks in
    t.wait_after_display_callbacks <- [];
    List.iter (Reversed_list.rev callbacks) ~f:(fun callback ->
      Effect.Expert.handle (Effect.Private.Callback.respond_to callback ()))
  ;;

  let has_after_display_events t =
    not (Reversed_list.is_empty t.wait_after_display_callbacks)
  ;;
end
