open! Core_kernel.Std
open! Incr_dom.Std

module Model : sig
  type t
  val cutoff : t -> t -> bool
  val create : int -> t
end

include App_intf.S_derived with module Model := Model
