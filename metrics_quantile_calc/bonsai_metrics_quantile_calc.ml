open! Core
module Buckets = Prometheus_instrumentation_collector_histogram_buckets.Buckets

let quantile_indexes ~quantiles ~len =
  List.map quantiles ~f:(fun q -> q *. Float.of_int len |> Int.of_float)
  |> List.sort ~compare:[%compare: int]
;;

let list_quantiles ~quantiles data =
  match data with
  | [] -> Or_error.error_string "No data provided."
  | _ ->
    let sorted_data = List.sort data ~compare:Time_ns.Span.compare |> List.to_array in
    quantile_indexes ~quantiles ~len:(List.length data)
    |> List.map ~f:(fun i -> sorted_data.(i))
    |> Ok
;;

let lre_quantiles ~quantiles ~counts =
  let total_count = Array.sum (module Int) counts ~f:Fn.id in
  match total_count with
  | 0 -> Or_error.error_string "Histogram is empty"
  | _ ->
    let counts_with_index = Array.mapi counts ~f:(fun i v -> i, v) |> Array.to_list in
    quantile_indexes ~quantiles ~len:total_count
    |> List.map ~f:(fun target_i ->
      List.fold_until
        counts_with_index
        ~init:0
        ~f:(fun cumulative_count (bucket_i, bucket_count) ->
          let lower_bound_count = cumulative_count in
          let upper_bound_count = cumulative_count + bucket_count in
          if bucket_count <> 0
             && lower_bound_count <= target_i
             && target_i < upper_bound_count
          then Stop bucket_i
          else Continue upper_bound_count)
        ~finish:(fun _ -> Array.length counts - 1))
    |> Ok
;;
