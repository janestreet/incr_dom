open! Core_kernel.Std
open! Import

module Model : sig
  type t = { entries : Entry.Model.t Entry_id.Map.t
           ; focus : (Entry_id.t * Focus_point.t option) option
           ; search_string : string
           }
  [@@deriving sexp]
end

module Action : sig
  type t [@@deriving sexp]
  val kick_all : t
  val kick_n : int -> t
  val should_log : t -> bool
  val apply : t -> schedule:(t -> unit) -> Model.t -> Model.t
  val nop : t
end

val example : entries:int -> Model.t

val view : Model.t Incr.t -> schedule:(Action.t -> unit) -> Vdom.Node.t Incr.t
