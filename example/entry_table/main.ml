open! Core_kernel.Std
open! Async_kernel.Std
open! Import

let () =
  Start_app.simple
    (module Entries)
    ~initial_model:(Entries.example ~entries:1000)
