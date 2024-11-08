open! Core
module Buckets = Prometheus_instrumentation_collector_histogram_buckets.Buckets

(** Various timings have different distributions:

   - The entire Incr_dom frame loops takes between .1ms to 50ms
   - A lot of stabilizations take no time, but most are somewhere between
     5us and 10ms
   - Patching vdom, apply actions, etc are typically between 10us and 10ms
   - Browser long tasks are at least 50ms, and may be way longer
   - RPC response timings are at least a few ms, and could take seconds.

  The timings we receive will always be rounded to the nearest 5us or 100us, depending on
  whether our app's headers have enabled cross-origin isolation.
*)

val fg_buckets_for_short_frequent_things : Time_ns.Span.t Buckets.t
val fg_buckets_for_rare_long_things : Time_ns.Span.t Buckets.t
val bg_buckets_for_short_frequent_things : Time_ns.Span.t Buckets.t
val bg_buckets_for_rare_long_things : Time_ns.Span.t Buckets.t
