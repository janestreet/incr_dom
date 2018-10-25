open! Core_kernel
open! Async_kernel
open! Import

let () =
  Start_app.component_old_do_not_use
    (module Entries)
    ~initial_model:(Entries.example ~entries:1000)
;;
