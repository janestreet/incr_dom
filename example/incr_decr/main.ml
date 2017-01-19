open! Core_kernel.Std
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.simple
    (module Counters)
    ~initial_model:(Counters.Model.Fields.create ~counters:(Int.Map.singleton 0 13))
