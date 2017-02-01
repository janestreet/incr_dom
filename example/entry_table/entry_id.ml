open! Core_kernel
open! Import

include Int
let create  = Fn.id
let id_string i =
  sprintf "entry_id:%d" i
