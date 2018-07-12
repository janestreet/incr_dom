open! Core_kernel
open! Incr_dom

module Model : sig
  type t

  val cutoff : t -> t -> bool

  val create : int -> t
end

include App_intf.S_derived with module Model := Model
