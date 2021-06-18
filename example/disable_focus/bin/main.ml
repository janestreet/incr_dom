open! Core
open! Incr_dom
open! Js_of_ocaml
module App = Disable_focus_example_lib.App

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:App.initial_model
;;
