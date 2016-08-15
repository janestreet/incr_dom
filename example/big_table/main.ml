open! Core_kernel.Std
open! Incr_dom.Std

let () =
  Start_app.imperative
    (module Table)
    ~initial_state:(Table.init ())
