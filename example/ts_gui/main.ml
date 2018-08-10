open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () = Start_app.component ~debug:true ~initial_model:(App.init ()) (module App)
