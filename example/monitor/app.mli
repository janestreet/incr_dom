open! Core_kernel
open! Async_kernel.Std
open! Incr_dom

include App_intf.S_simple

val init
  :  ?init_loc:string
  -> Monitor.t
  -> stop : unit Ivar.t
  -> Model.t
