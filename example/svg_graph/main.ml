open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  let counters =
    List.range 0 5
    |> List.map ~f:(fun k -> k, Random.int_incl 0 10)
    |> Int.Map.of_alist_exn
  in
  Start_app.start
    (module Counters)
    ~bind_to_element_with_id:"app"
    ~initial_model:(Counters.Model.Fields.create ~counters)
;;
