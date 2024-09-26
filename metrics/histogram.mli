open! Core

module Buckets : sig
  include module type of Prometheus_instrumentation_collector_histogram_buckets.Buckets

  val bucket_ranges : 'a t -> ('a * 'a) list
end

(** A JSOO-compatible histogram type that has the same semantics as
    {!Prometheus_instrumentation_collector.Histogram.t}. *)
type 'a t =
  { buckets : 'a Buckets.t
  ; counts : int array
  ; mutable sum : 'a
  ; mutable min : 'a option
  ; mutable max : 'a option
  }
[@@deriving sexp_of, equal]

val create : buckets:'a Buckets.t -> unit -> 'a t

(** [observe t datum] adds a recording of [datum] to the histogram [t] with frequency one.
    It finds the bucket in which the value observed resides and increments only that bucket. *)
val observe : 'a t -> 'a -> unit

(** Returns a copy of the histogram that can be mutated separately. *)
val clone : 'a t -> 'a t

module type S = sig
  type t [@@deriving compare, equal, sexp_of, quickcheck]

  val zero : t
  val ( + ) : t -> t -> t
  val scale_int_possibly_overflow_for_tests : t -> int -> t
end

val module_of_histogram : 'a Buckets.Type.t -> (module S with type t = 'a)

module For_sexp : sig
  type 'a outer_t = 'a t

  type 'a t =
    { boundaries : 'a array
    ; counts : int array
    ; sum : 'a
    ; min : 'a option
    ; max : 'a option
    }

  val t_of_outer_t : 'a outer_t -> 'a t
end
