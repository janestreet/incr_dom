open! Core_kernel.Std
open! Async_kernel.Std
open! Import

let () =
  Start_app.simple
    (module Entries)
    ~initial_state:(Entries.example ~entries:1000)
