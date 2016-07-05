open! Core_kernel.Std
open! Import

include Int
let create  = Fn.id
let id_string i =
  sprintf "entry_id:%d" i
