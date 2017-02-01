open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.derived
    ~initial_model:(App.init ())
    (module App)

