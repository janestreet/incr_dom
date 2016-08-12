open! Core_kernel.Std
open! Async_kernel.Std
open! Import

let () =
  Start_app.start
    (module Entries)
    ~initial_state:(Entries.example ~entries:1000)
    ~on_startup:(fun ~schedule _ ->
      Js_misc.scroll ();
      every (Time_ns.Span.of_sec 1.)  (fun () -> schedule (Entries.Action.kick_n 150));
    )
    ~project_immutable_summary:(fun model -> Option.map model.focus ~f:fst)
    ~on_display:(fun ~schedule:_ ~old:old_focus model ->
      let new_focus = Option.map model.focus ~f:fst in
      if old_focus <> new_focus then (Js_misc.scroll ())
    )
