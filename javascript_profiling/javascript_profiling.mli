(* [record name ~f] will call the function [f] and display
   the span for its execution in the chrome profiler. *)
val record : string -> f:(unit -> 'a) -> 'a
val clear_marks : unit -> unit
val clear_measures : unit -> unit

module Manual : sig
  val mark : string -> unit
  val measure : name:string -> start:string -> end_:string -> unit
end
