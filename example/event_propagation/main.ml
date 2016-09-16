open! Core_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml

let () =
  Start_app.simple (module App) ~initial_model:(App.create ())
