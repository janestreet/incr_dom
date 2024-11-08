open! Core
module Buckets = Prometheus_instrumentation_collector_histogram_buckets.Buckets

let fg_buckets_for_short_frequent_things =
  Buckets.Expert.create_exn
    Span
    (Nonempty_list.of_list_exn
       (List.concat
          [ [ Time_ns.Span.of_int_us 5 ]
          ; (List.init 799) ~f:(fun i -> Time_ns.Span.(of_int_us 25 + of_int_us (i * 25)))
          ; (List.init 100) ~f:(fun i -> Time_ns.Span.(of_int_ms 20 + of_int_ms i))
          ; (List.init 38) ~f:(fun i -> Time_ns.Span.(of_int_ms 120 + of_int_ms (i * 10)))
          ; (List.init 29) ~f:(fun i -> Time_ns.Span.(of_int_ms 500 + of_int_ms (i * 50)))
          ; (List.init 9) ~f:(fun i -> Time_ns.Span.(of_int_sec 2 + of_int_ms (i * 250)))
          ]))
;;

let bg_buckets_for_short_frequent_things =
  Buckets.Expert.create_exn
    Span
    (Nonempty_list.of_list_exn
       [ Time_ns.Span.of_int_us 5
       ; Time_ns.Span.of_int_us 100
       ; Time_ns.Span.of_int_us 250
       ; Time_ns.Span.of_int_us 500
       ; Time_ns.Span.of_int_ms 1
       ; Time_ns.Span.of_int_ms 2
       ; Time_ns.Span.of_int_ms 3
       ; Time_ns.Span.of_int_ms 4
       ; Time_ns.Span.of_int_ms 5
       ; Time_ns.Span.of_int_ms 10
       ; Time_ns.Span.of_int_ms 15
       ; Time_ns.Span.of_int_ms 20
       ; Time_ns.Span.of_int_ms 25
       ; Time_ns.Span.of_int_ms 50
       ; Time_ns.Span.of_int_ms 100
       ; Time_ns.Span.of_int_ms 250
       ; Time_ns.Span.of_int_ms 500
       ; Time_ns.Span.of_int_sec 1
       ])
;;

let fg_buckets_for_rare_long_things =
  Buckets.Expert.create_exn
    Span
    (Nonempty_list.of_list_exn
       (List.concat
          [ [ Time_ns.Span.of_int_ms 50 ]
          ; (List.init 449) ~f:(fun i -> Time_ns.Span.(of_int_ms 51 + of_int_ms i))
          ; (List.init 100) ~f:(fun i -> Time_ns.Span.(of_int_ms 500 + of_int_ms (i * 5)))
          ; (List.init 100) ~f:(fun i -> Time_ns.Span.(of_int_sec 1 + of_int_ms (i * 10)))
          ; (List.init 160) ~f:(fun i -> Time_ns.Span.(of_int_sec 2 + of_int_ms (i * 50)))
          ]))
;;

let bg_buckets_for_rare_long_things =
  Buckets.Expert.create_exn
    Span
    (Nonempty_list.of_list_exn
       [ Time_ns.Span.of_int_ms 50
       ; Time_ns.Span.of_int_ms 60
       ; Time_ns.Span.of_int_ms 70
       ; Time_ns.Span.of_int_ms 80
       ; Time_ns.Span.of_int_ms 90
       ; Time_ns.Span.of_int_ms 100
       ; Time_ns.Span.of_int_ms 125
       ; Time_ns.Span.of_int_ms 150
       ; Time_ns.Span.of_int_ms 175
       ; Time_ns.Span.of_int_ms 200
       ; Time_ns.Span.of_int_ms 225
       ; Time_ns.Span.of_int_ms 250
       ; Time_ns.Span.of_int_ms 300
       ; Time_ns.Span.of_int_ms 400
       ; Time_ns.Span.of_int_ms 500
       ; Time_ns.Span.of_int_sec 1
       ; Time_ns.Span.of_int_sec 2
       ; Time_ns.Span.of_int_sec 3
       ])
;;
