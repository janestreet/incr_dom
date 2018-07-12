open! Core_kernel
open! Import

type t

include Identifiable with type t := t

val create : int list -> t
