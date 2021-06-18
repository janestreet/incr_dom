open! Js_of_ocaml
open! Core
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.start
    ~bind_to_element_with_id:"app"
    ~initial_model:Todo.Model.empty
    (module Todo)
;;
