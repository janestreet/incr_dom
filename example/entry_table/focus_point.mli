open! Core_kernel.Std
open! Import

type t
include Identifiable with type t := t
val create : int list -> t
