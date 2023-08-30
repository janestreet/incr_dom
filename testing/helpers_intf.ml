open! Core
open! Import

module type S = sig
  type model
  type action
  type state

  val show_view : ?selector:string -> unit -> unit
  val show_model : unit -> unit
  val set_model : model -> unit
  val do_actions : action list -> unit
  val perform_update : unit -> unit
  val click_on : selector:string -> unit
  val input_text : selector:string -> text:string -> unit
end

module type Helpers = sig
  module type S = S

  val make
    :  ('model, 'action, 'state) Driver.t
    -> (module S
          with type model = 'model
           and type action = 'action
           and type state = 'state)
end
