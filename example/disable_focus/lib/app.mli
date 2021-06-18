open! Core
open! Async_kernel
open Incr_dom
include App_intf.S with type State.t = unit and type Model.t = unit

val initial_model : Model.t
