open! Core
module Histogram_bucket_definitions = Histogram_bucket_definitions
module Histogram = Histogram

module For_debugging = struct
  let collect_all_timings = ref false

  let%expect_test "collect_all_timings must be off by default" =
    assert (not !collect_all_timings)
  ;;
end

let document_is_hidden = ref false
let num_backgrounding_changes = ref 0

module Over_time = struct
  module Collected_at = struct
    module Why = struct
      type t =
        | Bonsai_started
        | On_interval
        | Tab_hidden
        | Tab_shown
        | Page_was_closed
      [@@deriving sexp_of, string]
    end

    type t =
      { why : Why.t
      ; timestamp : Time_ns.t
      ; time_since_navigation_start : Time_ns.Span.t
      }
    [@@deriving sexp_of]

    let create why =
      let timestamp, time_since_navigation_start =
        if am_running_test
        then Time_ns.epoch, Time_ns.Span.zero
        else
          ( Time_ns.now ()
          , Time_ns.Span.of_int63_ns (Time_now.nanosecond_counter_for_timing ()) )
      in
      { why; timestamp; time_since_navigation_start }
    ;;
  end

  module Snapshot = struct
    type 'a t =
      { delta : 'a (** [delta = cumulative] when [reason = First_contentful_paint] *)
      ; cumulative : 'a
      ; collected_at : Collected_at.t
      }
    [@@deriving sexp_of]
  end

  module Cumulative_only_snapshot = struct
    type 'a t =
      { cumulative : 'a
      ; collected_at : Collected_at.t
      }
    [@@deriving sexp_of]

    let of_snapshot { Snapshot.cumulative; collected_at; _ } =
      { cumulative; collected_at }
    ;;
  end
end

module One_off = struct
  module Collected_at = struct
    type t =
      { timestamp : Time_ns.t
      ; time_since_navigation_start : Time_ns.Span.t
      ; was_backgrounded : bool
      }
    [@@deriving sexp_of]

    let create ~was_backgrounded =
      let timestamp, time_since_navigation_start =
        if am_running_test
        then Time_ns.epoch, Time_ns.Span.zero
        else
          ( Time_ns.now ()
          , Time_ns.Span.of_int63_ns (Time_now.nanosecond_counter_for_timing ()) )
      in
      { was_backgrounded; timestamp; time_since_navigation_start }
    ;;
  end
end

module Timing_histograms = struct
  module Kind = struct
    module Input = struct
      type t =
        | Telemetry_idle_callback
        | Bonsai_whole_frame_loop
        | Bonsai_stabilization_update_visibility
        | Bonsai_stabilization_clock
        | Bonsai_stabilization_action
        | Bonsai_stabilization_after_apply_actions
        | Bonsai_update_visibility
        | Bonsai_apply_action
        | Bonsai_diff_vdom
        | Bonsai_patch_vdom
        | Bonsai_display_handlers
        | Browser_long_task
        | Bonsai_start_of_frame_to_start_of_next_frame
        | Bonsai_end_of_frame_to_start_of_next_frame
      [@@deriving string, sexp_of, equal, compare, hash, enumerate]
    end

    module Aggregated = struct
      type t = Bonsai_stabilization_all
      [@@deriving string, sexp_of, equal, compare, hash, enumerate]
    end

    module T = struct
      type t =
        | Input of Input.t
        | Aggregated of Aggregated.t
      [@@deriving equal, compare, hash, enumerate]

      let sexp_of_t = function
        | Aggregated x -> Aggregated.sexp_of_t x
        | Input x -> Input.sexp_of_t x
      ;;
    end

    let aggregations = function
      | Input.Bonsai_apply_action
      | Telemetry_idle_callback
      | Bonsai_whole_frame_loop
      | Bonsai_update_visibility
      | Bonsai_diff_vdom
      | Bonsai_patch_vdom
      | Bonsai_display_handlers
      | Browser_long_task
      | Bonsai_start_of_frame_to_start_of_next_frame
      | Bonsai_end_of_frame_to_start_of_next_frame -> []
      | Bonsai_stabilization_update_visibility
      | Bonsai_stabilization_clock
      | Bonsai_stabilization_action
      | Bonsai_stabilization_after_apply_actions ->
        [ Aggregated.Bonsai_stabilization_all ]
    ;;

    include T
    include Comparator.Make (T)

    let to_string = function
      | Input x -> Input.to_string x
      | Aggregated x -> Aggregated.to_string x
    ;;

    (* This needs:
     - A test to make sure that everything serializes and deserializes with no duplicates. *)
    let of_string = function
      | "Bonsai_stabilization_all" -> Aggregated Bonsai_stabilization_all
      | x -> Input (Input.of_string x)
    ;;

    let foreground_buckets = function
      | Input Bonsai_whole_frame_loop
      | Input Telemetry_idle_callback
      | Aggregated Bonsai_stabilization_all
      | Input Bonsai_stabilization_update_visibility
      | Input Bonsai_stabilization_clock
      | Input Bonsai_stabilization_action
      | Input Bonsai_stabilization_after_apply_actions
      | Input Bonsai_update_visibility
      | Input Bonsai_apply_action
      | Input Bonsai_diff_vdom
      | Input Bonsai_patch_vdom
      | Input Bonsai_display_handlers
      | Input Bonsai_start_of_frame_to_start_of_next_frame
      | Input Bonsai_end_of_frame_to_start_of_next_frame ->
        (* We use few buckets here, because having a thousand would clobber tests. *)
        Histogram_bucket_definitions.(
          if am_running_test
          then bg_buckets_for_short_frequent_things
          else fg_buckets_for_short_frequent_things)
      | Input Browser_long_task ->
        Histogram_bucket_definitions.(
          if am_running_test
          then bg_buckets_for_rare_long_things
          else fg_buckets_for_rare_long_things)
    ;;

    let background_buckets = function
      | Input Bonsai_whole_frame_loop
      | Input Telemetry_idle_callback
      | Aggregated Bonsai_stabilization_all
      | Input Bonsai_stabilization_update_visibility
      | Input Bonsai_stabilization_clock
      | Input Bonsai_stabilization_action
      | Input Bonsai_stabilization_after_apply_actions
      | Input Bonsai_update_visibility
      | Input Bonsai_apply_action
      | Input Bonsai_diff_vdom
      | Input Bonsai_patch_vdom
      | Input Bonsai_display_handlers
      | Input Bonsai_start_of_frame_to_start_of_next_frame
      | Input Bonsai_end_of_frame_to_start_of_next_frame ->
        Histogram_bucket_definitions.bg_buckets_for_short_frequent_things
      | Input Browser_long_task ->
        Histogram_bucket_definitions.bg_buckets_for_rare_long_things
    ;;
  end

  module Snapshots = struct
    type t =
      | On_interval_foregrounded_only of
          Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
      | On_init_visibility_change_or_hide of
          { foregrounded : Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
          ; backgrounded : Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
          }
    [@@deriving sexp_of]
  end

  module Tracker = struct
    type t =
      { collector : Time_ns.Span.t Histogram.t
      ; mutable all_timings_for_debugging : Time_ns.Span.t list
      }

    let create buckets =
      { collector = Histogram.create ~buckets (); all_timings_for_debugging = [] }
    ;;

    let observe t datum =
      if !For_debugging.collect_all_timings
      then t.all_timings_for_debugging <- datum :: t.all_timings_for_debugging;
      Histogram.observe t.collector datum
    ;;

    let snapshot ~why t =
      { Over_time.Cumulative_only_snapshot.cumulative = Histogram.clone t.collector
      ; collected_at = Over_time.Collected_at.create why
      }
    ;;
  end

  module For_one = struct
    type t =
      { foreground : Tracker.t
      ; background : Tracker.t
      }

    let create ~foreground_buckets ~background_buckets =
      { foreground = Tracker.create foreground_buckets
      ; background = Tracker.create background_buckets
      }
    ;;

    let observe t datum =
      let fg_or_bg = if !document_is_hidden then t.background else t.foreground in
      Tracker.observe fg_or_bg datum
    ;;

    let take_snapshots { foreground; background } ~why =
      let snapshot = Tracker.snapshot ~why in
      match why with
      | On_interval -> Snapshots.On_interval_foregrounded_only (snapshot foreground)
      | Bonsai_started | Page_was_closed | Tab_hidden | Tab_shown ->
        On_init_visibility_change_or_hide
          { foregrounded = snapshot foreground; backgrounded = snapshot background }
    ;;
  end

  let the_one_and_only = Hashtbl.create (module Kind)

  let observe' kind span =
    Hashtbl.update the_one_and_only kind ~f:(fun for_one ->
      let for_one =
        match for_one with
        | None ->
          For_one.create
            ~foreground_buckets:(Kind.foreground_buckets kind)
            ~background_buckets:(Kind.background_buckets kind)
        | Some for_one -> for_one
      in
      For_one.observe for_one span;
      for_one)
  ;;

  let observe input_kind span =
    observe' (Input input_kind) span;
    List.iter (Kind.aggregations input_kind) ~f:(fun aggregation ->
      observe' (Aggregated aggregation) span)
  ;;

  let take_snapshots ~why =
    Hashtbl.to_alist the_one_and_only
    |> List.Assoc.map ~f:(fun x -> For_one.take_snapshots x ~why)
  ;;
end

module Counters = struct
  module Kind = struct
    module T = struct
      type t =
        | Incr_node_input
        | Incr_node_value
        | Incr_node_result
        | Incr_node_lifecycle
        | Incr_node_empty_lifecycle
        | Incr_node_model
        | Incr_node_model_and_input
        | Incr_node_switch_model
        | Incr_node_assoc_key
        | Incr_node_assoc_input
        | Incr_node_assoc_results
        | Incr_node_assoc_lifecycles
        | Incr_node_assoc_inputs
        | Incr_node_path
        | Incr_node_lifecycle_apply_action_pair
        | Incr_skipped_stabilizations
      [@@deriving string, sexp_of, equal, compare, hash]
    end

    include T
    include Comparator.Make (T)
  end

  module Snapshots = struct
    type t =
      { all : int Over_time.Snapshot.t
      ; foregrounded : int Over_time.Snapshot.t
      }
    [@@deriving sexp_of]
  end

  module Tracker = struct
    type t =
      { mutable counter : int
      ; mutable last_sent : int
      }

    let create () = { counter = 0; last_sent = 0 }
    let incr t = t.counter <- t.counter + 1

    let snapshot ~why t =
      let delta = t.counter - t.last_sent in
      t.last_sent <- t.counter;
      { Over_time.Snapshot.delta
      ; cumulative = t.counter
      ; collected_at = Over_time.Collected_at.create why
      }
    ;;
  end

  module For_one = struct
    type t =
      { foreground : Tracker.t
      ; all : Tracker.t
      }

    let create () =
      let foreground = Tracker.create () in
      let all = Tracker.create () in
      { foreground; all }
    ;;

    let observe t =
      if not !document_is_hidden then Tracker.incr t.foreground;
      Tracker.incr t.all
    ;;

    let take_snapshots { foreground; all } ~why =
      { Snapshots.all = Tracker.snapshot ~why all
      ; foregrounded = Tracker.snapshot ~why foreground
      }
    ;;
  end

  let the_one_and_only = Hashtbl.create (module Kind)

  let observe kind =
    Hashtbl.update the_one_and_only kind ~f:(fun for_one ->
      let for_one =
        match for_one with
        | None -> For_one.create ()
        | Some for_one -> for_one
      in
      For_one.observe for_one;
      for_one)
  ;;

  let take_snapshots ~why =
    Hashtbl.to_alist the_one_and_only
    |> List.Assoc.map ~f:(fun x -> For_one.take_snapshots x ~why)
  ;;
end

module One_off_timings = struct
  module Kind = struct
    module T = struct
      type t =
        | Bonsai_graph_application
        | Bonsai_preprocess
        | Bonsai_gather
        | Incr_app_creation
        | First_stabilization
        | Mount_initial_dom
      [@@deriving sexp_of, equal, string, compare, hash]
    end

    include T
    include Comparator.Make (T)
  end

  module Reported = struct
    type t =
      | Not_reported
      | Reported
  end

  module Timing = struct
    type t =
      { kind : Kind.t
      ; value : Time_ns.Span.t
      ; collected_at : One_off.Collected_at.t
      }
    [@@deriving sexp_of]
  end

  let the_one_and_only = Hashtbl.create (module Kind)

  let observe kind value =
    let was_backgrounded = !document_is_hidden in
    Hashtbl.set
      the_one_and_only
      ~key:kind
      ~data:
        ( { Timing.kind
          ; value
          ; collected_at = One_off.Collected_at.create ~was_backgrounded
          }
        , Reported.Not_reported )
  ;;

  let all_values () = Hashtbl.data the_one_and_only |> List.map ~f:fst

  let new_values () =
    Hashtbl.data the_one_and_only
    |> List.filter_map ~f:(fun (v, reported) ->
      match reported with
      | Reported -> None
      | Not_reported ->
        Hashtbl.change the_one_and_only v.kind ~f:(function
          | None -> None
          | Some (v, _) -> Some (v, Reported));
        Some v)
  ;;
end

let () =
  Bonsai.Private.Annotate_incr.on_incr_annotation (fun kind _ ->
    let our_kind =
      match kind with
      | Bonsai.Private.Annotate_incr.Kind.Input -> Counters.Kind.Incr_node_input
      | Value -> Incr_node_value
      | Result -> Incr_node_result
      | Lifecycle -> Incr_node_lifecycle
      | Empty_lifecycle -> Incr_node_empty_lifecycle
      | Model -> Incr_node_model
      | Model_and_input -> Incr_node_model_and_input
      | Switch_model -> Incr_node_switch_model
      | Assoc_key -> Incr_node_assoc_key
      | Assoc_input -> Incr_node_assoc_input
      | Assoc_results -> Incr_node_assoc_results
      | Assoc_lifecycles -> Incr_node_assoc_lifecycles
      | Assoc_inputs -> Incr_node_assoc_input
      | Path -> Incr_node_path
      | Lifecycle_apply_action_pair -> Incr_node_lifecycle_apply_action_pair
    in
    Counters.observe our_kind)
;;

module For_debugging_histograms = struct
  let print_quantiles_for_single_tracker
    ?exclude_timings_lt
    (t : Timing_histograms.Tracker.t)
    =
    let module Quantile_calc = Bonsai_metrics_quantile_calc in
    let range_midpoint (a, b) =
      Time_ns.Span.(if b = max_value_representable then a else (a + b) / 2.)
    in
    let quantiles = [ 0.1; 0.25; 0.5; 0.75; 0.9; 0.95; 0.99; 0.999 ] in
    let all_bucket_ranges = Histogram.Buckets.bucket_ranges t.collector.buckets in
    let samples, counts, bucket_ranges =
      match exclude_timings_lt with
      | Some filter_lt ->
        let all =
          List.filter t.all_timings_for_debugging ~f:(fun t ->
            Time_ns.Span.(t > filter_lt))
        in
        let min_bucket_index, _ =
          (* This might error, but that's ok because this function is only ever called
             from the console by a developer. *)
          Array.findi_exn
            (Histogram.Buckets.boundaries_excluding_infinity t.collector.buckets)
            ~f:(fun _i bucket_max -> Time_ns.Span.(bucket_max > filter_lt))
        in
        let counts = Array.subo t.collector.counts ~pos:min_bucket_index in
        let bucket_ranges =
          List.sub
            all_bucket_ranges
            ~pos:min_bucket_index
            ~len:(List.length all_bucket_ranges - min_bucket_index)
        in
        all, counts, bucket_ranges
      | None -> t.all_timings_for_debugging, t.collector.counts, all_bucket_ranges
    in
    match Quantile_calc.lre_quantiles ~quantiles ~counts with
    | Error e ->
      print_s [%message "Could not calculate histogram quantiles" (e : Error.t)]
    | Ok histogram_quantile_buckets ->
      let histogram_quantile_ranges =
        List.map histogram_quantile_buckets ~f:(fun bucket_i ->
          List.nth_exn bucket_ranges bucket_i)
      in
      (match Quantile_calc.list_quantiles ~quantiles samples with
       | Error e ->
         print_s
           [%message
             "Could not calculate sample quantiles"
               (e : Error.t)
               "Make sure you've run `bonsaiTelemetryEnableDebugging(true)`"];
         List.zip_exn quantiles histogram_quantile_ranges
         |> List.iter ~f:(fun (quantile, range) ->
           let approx = range_midpoint range in
           print_s
             [%message
               (quantile : float)
                 (approx : Time_ns.Span.t)
                 (range : Time_ns.Span.t * Time_ns.Span.t)])
       | Ok true_quantiles ->
         List.zip_exn (List.zip_exn quantiles histogram_quantile_ranges) true_quantiles
         |> List.iter ~f:(fun ((quantile, range), true_v) ->
           let approx = range_midpoint range in
           print_s
             [%message
               (quantile : float)
                 (true_v : Time_ns.Span.t)
                 (approx : Time_ns.Span.t)
                 (range : Time_ns.Span.t * Time_ns.Span.t)]))
  ;;

  let print_quantiles kind =
    match Hashtbl.find Timing_histograms.the_one_and_only kind with
    | None -> print_s [%message "No data collected yet" (kind : Timing_histograms.Kind.t)]
    | Some { foreground; background } ->
      print_endline "For foregrounded data:";
      print_quantiles_for_single_tracker foreground;
      print_endline "";
      print_endline "For backgrounded data:";
      print_quantiles_for_single_tracker background;
      print_endline "";
      print_endline "For foregrounded data (filtering insignificant):";
      print_quantiles_for_single_tracker
        ~exclude_timings_lt:(Time_ns.Span.of_int_us 5)
        foreground;
      print_endline "";
      print_endline "For backgrounded data (filtering insignificant):";
      print_quantiles_for_single_tracker
        ~exclude_timings_lt:(Time_ns.Span.of_int_us 5)
        background;
      print_endline ""
  ;;

  let print_data kind =
    match Hashtbl.find Timing_histograms.the_one_and_only kind with
    | None -> ()
    | Some { foreground; background = _ } ->
      let bucket_ranges = Histogram.Buckets.bucket_ranges foreground.collector.buckets in
      let ranges_and_counts =
        List.zip_exn bucket_ranges (Array.to_list foreground.collector.counts)
        |> List.filter ~f:(fun (_, count) -> count > 0)
      in
      print_s [%sexp (ranges_and_counts : ((Time_ns.Span.t * Time_ns.Span.t) * int) list)]
  ;;

  let collect_all_timings = For_debugging.collect_all_timings

  let analyze_histogram_quantile_accuracy () =
    List.iter Timing_histograms.Kind.all ~f:(fun kind ->
      print_endline [%string "Analysis for: %{kind#Timing_histograms.Kind}"];
      print_quantiles kind)
  ;;
end

module Private = struct
  let set_document_is_hidden is_hidden =
    if not (Bool.equal is_hidden !document_is_hidden)
    then (
      document_is_hidden := is_hidden;
      incr num_backgrounding_changes)
  ;;

  let num_backgrounding_changes () = !num_backgrounding_changes
end

module For_testing = struct
  let clear () =
    Hashtbl.clear Timing_histograms.the_one_and_only;
    Hashtbl.clear Counters.the_one_and_only;
    Hashtbl.clear One_off_timings.the_one_and_only
  ;;

  let set_document_is_hidden = Private.set_document_is_hidden
end
