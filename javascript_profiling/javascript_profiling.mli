open! Core

(** The browser's `Performance` API provides us with a performant way to measure and record
    timespans and events of interest. These will be broadcast to `PerformanceObserver`s,
    and will appear on the "timing" track of any Chrome performance traces.

    The `javascript_profiling` library provides bindings to this API.
*)
module Timer : sig
  type t
  type measurement

  val start : unit -> t
  val stop : t -> measurement
  val record : (unit -> 'a) -> 'a * measurement

  module Duration : sig
    (** If the tab was backgrounded or unbackgrounded between the start and stop
        timestamp, the measured duration will be very unreliable.

        Background status is determined by referencing [Bonsai_metrics.Private.num_backgrounding_changes]. *)
    type t =
      | Ok of Time_ns.Span.t
      | Backgrounding_changed_unreliable of Time_ns.Span.t
    [@@deriving sexp_of]
  end

  val duration : measurement -> Duration.t
end

module Dev_tools_color : sig
  type t =
    | Primary
    | Primary_light
    | Primary_dark
    | Secondary
    | Secondary_light
    | Secondary_dark
    | Tertiary
    | Tertiary_light
    | Tertiary_dark
    | Error
  [@@deriving sexp_of, string]
end

(** [mark] will create a mark on the performance timeline.

    If [prominent] (default false), the mark will be prominently displayed in the DevTools
    performance trace. *)
val mark : ?prominent:bool -> ?color:Dev_tools_color.t -> string -> unit

(** [measure name measurement] will create a measure on the Chrome performance timeline.

    If [Measure.duration measurement] is [None], this is a no-op. *)
val measure
  :  ?color:Dev_tools_color.t
  -> ?track:string
  -> string
  -> Timer.measurement
  -> unit

(** [clear_marks] will clear all marks from the buffer that
    powers Chrome's [getEntriesByType] API.

    If [name] is specified, only marks with name [name] will be cleared. Otherwise, all
    measures in the performance buffer will be cleared.

    If using [PerformanceObserver.observe] instead of [getEntriesByType], this should
    only cause issues if you've created your performance observer with the [buffered]
    option, and expect to observe entries prior to creation. *)
val clear_marks : ?name:string -> unit -> unit

(** [clear_all_measures] is like [clear_marks], but for measures. *)
val clear_measures : ?name:string -> unit -> unit

val time_since_navigation_start : unit -> Time_ns.Span.t
