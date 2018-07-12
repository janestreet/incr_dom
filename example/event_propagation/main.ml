open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () = Start_app.simple (module App) ~initial_model:(App.create ())
