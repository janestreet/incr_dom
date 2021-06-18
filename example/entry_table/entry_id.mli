open! Core
open! Import

type t

val create : int -> t

include Identifiable with type t := t

val id_string : t -> string
