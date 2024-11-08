open! Core

(** An ['a t] represents a partition of the values in ['a] into at least two intervals.
    Any value of type ['a] (except for NaNs if ['a = float]) belongs to exactly one
    bucket.

    The only use for this type is making histograms, and the buckets get turned into
    metrics with special [le] labels, corresponding to counts of events with values less
    than or equal to the given one. In particular:
    - floats and ints get converted directly into [le] values
    - time spans are converted into a (decimal) number of {e seconds} for the [le] label
    - A bucket is automatically added with an upper bound of [+Inf]

    Note that the [compare] and [equal] functions completely ignore the passed in
    comparison or equality function for ['a] and always use the standard comparison
    or equality function of whatever type ['a] actually represents (e.g. [Int.compare]).
*)
type 'a t [@@deriving compare, equal, sexp_of]

(** Describes the allowable types for bucket boundaries *)
module Type : sig
  type 'a t =
    | Float : float t
    | Int : int t
    | Span : Time_ns.Span.t t
  [@@deriving sexp_of]
end

module Bucket_spec : sig
  (** A type used to make the helper functions ({!Linear}, {!Exponential}) generic over
      whether they produce buckets for floats or time spans *)

  (** A [('params, 'buckets) t] tells a creator function that parameters (e.g.
      [range_start]) are of type ['params] and it should produce buckets of type
      ['buckets]. *)
  type ('input_params, 'output_buckets) t =
    | Float : (float, float) t (** Specify [float]s to generate [float] buckets  *)
    | Seconds : (float, Time_ns.Span.t) t
    (** Specify numbers of seconds as [float]s but generate [Time_ns.Span.t] buckets *)
    | Span : (Time_ns.Span.t, Time_ns.Span.t) t
    (** Parameters are spans, output is spans. *)
    | Int : (int, int) t
    (** Parameters are specified in ints and rounded nearest from the values you would
        get had you used floats. You’ll get an error if two boundaries get rounded to
        the same int. *)
end

(** Buckets that serve as a reasonable default for users that don't want to customize the
    boundaries. *)
module Predefined : sig
  (** The default buckets are tailored to broadly measure the response time (in seconds)
      of a network service. Their inclusive upper bounds are:
      [[ 0.0001; 0.001; 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5; 5.; 10.; inf ]]

      If you're not sure where to start with histograms, use these buckets.

      This range isn't the right answer for most apps, so using a custom set of boundaries
      with a smaller range but denser placement will likely give you a more optimal
      querying experience. You can use the constructors below to create a custom range. *)
  val default : Time_ns.Span.t t

  (** Like {!default} but float (seconds) instead of time spans. *)
  val float : float t
end

(** Helper functions for creating uniformly spaced buckets. *)
module Linear : sig
  (** Creates [count + 1] buckets where the lowest bucket has an upper bound of
      [range_start] and the highest bucket has a lower bound of [range_end]. Equivalently,
      it results in [count] bucket boundaries where the first boundary is [range_start]
      and the last boundary is [range_end].

      Apart from the first and the last buckets that are infinite in size, the remaining
      [count - 1] buckets have equal sizes. You end up with [count + 1] buckets
      because we ensure that you have a final bucket with the value of [+inf] at
      the end (and the lower bucket starting at [-inf] is always implied by how histograms
      work).

      For example, [create_from_range Float ~range_start:1.0 ~range_end:5.0 ~count:5]
      results in the following buckets:
      {v
        (-inf, 1.0]
        (1.0,  2.0]
        (2.0,  3.0]
        (3.0,  4.0]
        (4.0,  5.0]
        (5.0, +inf)
      v}

      The function returns an error if:
      - [range_start >= range_end]
      - [count <= 1]
  *)
  val create_from_range
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> range_end:'param
    -> count:int
    -> 'out t Or_error.t

  (** Same as {!create_from_range}, but throws an exception instead of returning an error. *)
  val create_from_range_exn
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> range_end:'param
    -> count:int
    -> 'out t

  (** Creates [count + 1] buckets where the lowest bucket has an upper bound of
      [range_start] and each of the next buckets will have an upper bound of the previous
      upper bound, plus [step].

      The function returns an error if:
      - [step <= 0]
      - [count <= 1]
  *)
  val create_from_step
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> step:'param
    -> count:int
    -> 'out t Or_error.t

  (** Same as {!create_from_step}, but throws an exception instead of returning an error. *)
  val create_from_step_exn
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> step:'param
    -> count:int
    -> 'out t
end

(** Helper functions for creating buckets with exponentially increasing size. *)
module Exponential : sig
  (** Creates [count + 1] buckets where the lowest bucket has an upper bound
      of [range_start] and the second-highest bucket has an upper bound of [range_end]
      and the ratio between the sizes of any two consecutive buckets within the range
      is constant.

      For example, [create_from_range Float ~range_start:1. ~range_end:16. ~count:5]
      results in the following buckets:
      {v
        (-inf,  1.0]       (size = inf)
        (1.0,   2.0]       (size = 1)
        (2.0,   4.0]       (size = 2)
        (4.0,   8.0]       (size = 4)
        (8.0,  16.0]       (size = 8)
        (16.0, +inf)       (size = inf)
      v}

      The function returns an error if:
      - [range_start >= range_end]
      - [range_start <= 0]
      - [count <= 1]
  *)
  val create_from_range
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> range_end:'param
    -> count:int
    -> 'out t Or_error.t

  (** Same as {!create_from_range}, but throws an exception instead of returning an error. *)
  val create_from_range_exn
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> range_end:'param
    -> count:int
    -> 'out t

  (** Creates [count + 1] buckets, where the lowest bucket has an upper bound
      of [range_start] and each of the next buckets has an upper bound equal
      to the previous upper bound, times [factor].

      The function returns an error if:
      - [range_start <= 0]
      - [count <= 1]
      - [factor <= 1]
  *)
  val create_from_factor
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> factor:float
    -> count:int
    -> 'out t Or_error.t

  (** Same as {!create_from_factor}, but throws an exception instead of returning an error. *)
  val create_from_factor_exn
    :  ('param, 'out) Bucket_spec.t
    -> range_start:'param
    -> factor:float
    -> count:int
    -> 'out t
end

module Expert : sig
  (** Creates buckets with arbitrary boundaries, appending infinity to the list
      of boundaries if it’s not already there.

      If the last bucket isn’t [Float.infinity] (corresponding to [le="Inf+"] for
      a {!Histogram} metric), [Float.infinity] is automatically added at the end.

      For example a list [[1.0; 2.0; 3.0]] would result in the buckets:
      {v
        (-Inf to 1.0]; (1.0 to 2.0]; (2.0 to 3.0]; (3.0 to +Inf)
      v}

      The function returns an error if:
      - the list of boundaries is not strictly monotonically increasing (i.e. if there are
        consecutive elements [a] and [b] where [not (a < b)]);
      - the list of boundaries contains NaNs;
      - the list of boundaries is empty or only contains infinity. *)
  val create : 'a Type.t -> 'a Nonempty_list.t -> 'a t Or_error.t

  (** Same as {!create}, but raises an exception instead of returning an error. *)
  val create_exn : 'a Type.t -> 'a Nonempty_list.t -> 'a t

  (** Same as {!create}, but takes an array of boundaries instead of a list. *)
  val create_of_array : 'a Type.t -> 'a Array.t -> 'a t Or_error.t

  (** [create_seconds_exn floats] is equivalent to
      [create_exn Span (Nonempty_list.map ~f:Time_ns.Span.of_sec floats)] *)
  val create_seconds_exn : float Nonempty_list.t -> Time_ns.Span.t t
end

(** Boundaries for the buckets. When using these boundaries, one should implicitly add
    another bucket at [+Inf]. *)
val boundaries_excluding_infinity : 'a t -> 'a Array.t

val type_ : 'a t -> 'a Type.t
