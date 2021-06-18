(* [record name ~f] will call the function [f] and display
   the span for its execution in the chrome profiler. *)
val record : string -> f:(unit -> 'a) -> 'a

module Manual : sig
  val mark : string -> unit
  val measure : name:string -> start:string -> end_:string -> unit
end
