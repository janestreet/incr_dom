open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.start
    (module Counters)
    ~bind_to_element_with_id:"app"
    ~initial_model:(Counters.Model.Fields.create ~counters:(Int.Map.singleton 0 13))
;;
