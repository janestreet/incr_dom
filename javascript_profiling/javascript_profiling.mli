(** [record name ~f] will call the function [f] and display
    the span for its execution in the chrome profiler. *)
val record : string -> f:(unit -> 'a) -> 'a

(** If [name] is specified, only marks with name [name] will be cleared. Otherwise, all
    marks in the performance buffer will be cleared.

    Chrome writes marks / measures into 3 places:

    1. The Chrome DevTools Performance panel
    2. Any [PerformanceObserver]s listening to marks / measures
    3. An internal, unbounded buffer used to pair marks / measures, and for retrieval
    via [getEntriesByType].

    [clear_marks] only clears (3).

    If using [PerformanceObserver.observe] and not polling [getEntriesByType], potential
    issues caused by clearing include:

    - Clearing in between a "start" and "end" mark. This will throw an exception when
      calling "measure"
    - Not being able to pull existing marks / measures when using [observe] with the
      [buffered] option.
*)
val clear_marks : ?name:string -> unit -> unit

(** If [name] is specified, only measures with name [name] will be cleared. Otherwise, all
    measures in the performance buffer will be cleared.

    See [clear_marks] for more information on when you need to do this and what buffers
    this operation will affect. *)
val clear_measures : ?name:string -> unit -> unit

module Manual : sig
  val mark : string -> unit

  val measure
    :  name:string
    -> start:string
    -> end_:string
    -> Js_of_ocaml.PerformanceObserver.performanceEntry Js_of_ocaml.Js.t
end
