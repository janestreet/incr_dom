open! Core
module Histogram_bucket_definitions = Histogram_bucket_definitions
module Histogram = Histogram

(** This module is used for collecting metrics from the currently running
    Bonsai app. It uses a global store of values so that we don't need to thread a
    handle through all the places in Bonsai where we might want to record
    data.

    As a side effect, this module listens to the number of incremental nodes
    annotated by Bonsai. *)

module Over_time : sig
  module Collected_at : sig
    module Why : sig
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
  end

  module Snapshot : sig
    type 'a t =
      { delta : 'a (** [delta = cumulative] when [reason = First_contentful_paint] *)
      ; cumulative : 'a
      ; collected_at : Collected_at.t
      }
    [@@deriving sexp_of]
  end

  module Cumulative_only_snapshot : sig
    type 'a t =
      { cumulative : 'a
      ; collected_at : Collected_at.t
      }
    [@@deriving sexp_of]

    val of_snapshot : 'a Snapshot.t -> 'a t
  end
end

module One_off : sig
  module Collected_at : sig
    type t =
      { timestamp : Time_ns.t
      ; time_since_navigation_start : Time_ns.Span.t
      ; was_backgrounded : bool
      }
    [@@deriving sexp_of]
  end
end

module Timing_histograms : sig
  module Kind : sig
    module Input : sig
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
      [@@deriving string, sexp_of, equal, compare, enumerate]
    end

    module Aggregated : sig
      (** A [Kind.Aggregated.t] represents the union of some [Kind.Input.t]s,
          that we want to explcitly collect because aggregating the collected data
          is impractical. *)
      type t = Bonsai_stabilization_all
      [@@deriving string, sexp_of, equal, compare, enumerate]
    end

    type t =
      | Input of Input.t
      | Aggregated of Aggregated.t
    [@@deriving string, sexp_of, equal, compare, enumerate]
  end

  val observe : Kind.Input.t -> Time_ns.Span.t -> unit

  module Snapshots : sig
    type t =
      | On_interval_foregrounded_only of
          Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
      | On_init_visibility_change_or_hide of
          { foregrounded : Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
          ; backgrounded : Time_ns.Span.t Histogram.t Over_time.Cumulative_only_snapshot.t
          }
    [@@deriving sexp_of]
  end

  val take_snapshots : why:Over_time.Collected_at.Why.t -> (Kind.t * Snapshots.t) list
end

module Counters : sig
  module Kind : sig
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
    [@@deriving string, sexp_of, equal, compare]

    include Comparator.S with type t := t
  end

  val observe : Kind.t -> unit

  module Snapshots : sig
    type t =
      { all : int Over_time.Snapshot.t
      ; foregrounded : int Over_time.Snapshot.t
      }
    [@@deriving sexp_of]
  end

  val take_snapshots : why:Over_time.Collected_at.Why.t -> (Kind.t * Snapshots.t) list
end

module One_off_timings : sig
  module Kind : sig
    type t =
      | Bonsai_graph_application
      | Bonsai_preprocess
      | Bonsai_gather
      | Incr_app_creation
      | First_stabilization
      | Mount_initial_dom
    [@@deriving sexp_of, equal, string, compare]

    include Comparator.S with type t := t
  end

  module Timing : sig
    type t =
      { kind : Kind.t
      ; value : Time_ns.Span.t
      ; collected_at : One_off.Collected_at.t
      }
    [@@deriving sexp_of]
  end

  val observe : Kind.t -> Time_ns.Span.t -> unit
  val new_values : unit -> Timing.t list
  val all_values : unit -> Timing.t list
end

module For_debugging_histograms : sig
  val collect_all_timings : bool ref
  val analyze_histogram_quantile_accuracy : unit -> unit
  val print_data : Timing_histograms.Kind.t -> unit
end

module For_testing : sig
  val clear : unit -> unit
  val set_document_is_hidden : bool -> unit
end

module Private : sig
  val set_get_time_since_navigation_start : (unit -> Time_ns.Span.t) -> unit
  val set_document_is_hidden : bool -> unit
  val num_backgrounding_changes : unit -> int
end
