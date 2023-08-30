open! Core
open! Async_kernel
open! Import

type ('model, 'action, 'state) t

val create
  :  initial_model:'model
  -> sexp_of_model:('model -> Sexp.t)
  -> initial_state:'state
  -> (module Incr_dom.App_intf.S
        with type Action.t = 'action
         and type Model.t = 'model
         and type State.t = 'state)
  -> ('model, 'action, 'state) t

val model_var : ('model, _, _) t -> 'model Incr.Var.t
val view_incr : (_, _, _) t -> Vdom.Node.t Incr.t
val action_queue : (_, 'action, _) t -> 'action Queue.t
val perform_update : (_, _, _) t -> unit
val sexp_of_model : ('model, _, _) t -> 'model -> Sexp.t
val set_time : _ t -> Time_ns.t -> unit
