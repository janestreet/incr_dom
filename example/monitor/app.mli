open! Core
open! Async_kernel
open! Incr_dom
include App_intf.S

val init : ?init_loc:string -> Monitor.t -> stop:unit Ivar.t -> Model.t
