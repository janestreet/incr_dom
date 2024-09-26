open! Core
module Buckets = Prometheus_instrumentation_collector_histogram_buckets.Buckets

(** Compute a quantiles of a list.

    [quantiles] should each be between 0 and 1.

    The returned list of quantile values is in ascending order.

    Returns an error if data is empty. *)
val list_quantiles
  :  quantiles:float list
  -> Time_ns.Span.t list
  -> Time_ns.Span.t list Or_error.t

(** Given a LRE-encoded list of "count" buckets, return the indexes of buckets that
    contain each quantile, in ascending order.

    [quantiles] should each be between 0 and 1.

    Returns an error if data is empty. *)
val lre_quantiles : quantiles:float list -> counts:int array -> int list Or_error.t
