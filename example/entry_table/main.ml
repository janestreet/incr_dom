open! Core
open! Async_kernel
open! Import

let () =
  Start_app.start
    (module Entries)
    ~bind_to_element_with_id:"app"
    ~initial_model:(Entries.example ~entries:1000)
;;
