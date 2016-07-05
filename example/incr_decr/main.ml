open! Core_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml

let () =
  Start_app.start
    (module Counters)
    ~initial_state:(Counters.Model.Fields.create ~counters:(Int.Map.singleton 0 13))
    ~on_startup:(fun ~schedule:_ _ -> ())
    ~on_display:(fun ~schedule:_ _ _ -> ())
