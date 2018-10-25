open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.component_old_do_not_use (module App) ~initial_model:(App.Model.init ())
;;
