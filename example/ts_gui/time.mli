open Core_kernel

(** A time module intended to be used in constructing columns for a table view *)

type t = Time_ns.t

include Comparable.S with type t := t

val now : unit -> t
val to_string : t -> string

(** This function asserts false when called, but it won't be, since we don't presently
    allow editing of times. However, since it is being displayed, and all columns support
    editing (even though we don't actually edit all of them) this function must be
    implemented.
*)
val of_string : string -> t option
