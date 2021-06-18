open! Core
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.start
    ~bind_to_element_with_id:"app"
    ~debug:false
    ~initial_model:(App.init ())
    (module App)
;;
