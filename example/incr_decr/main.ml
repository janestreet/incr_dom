open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.component_old_do_not_use
    (module Counters)
    ~initial_model:(Counters.Model.Fields.create ~counters:(Int.Map.singleton 0 13))
;;
