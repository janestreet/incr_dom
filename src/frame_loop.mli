open! Core

(** [start_looping] will run [perform_update] immediately, then call it every frame
    ([requestAnimationFrame] or [setTimeout(f, 1000)], whichever happens sooner) unless
    [is_stopped] returns true. This "drives" a Bonsai / Incr_dom web app.

    [start_looping] returns immediately after the first [perform_update] call finishes. *)
val start_looping : is_stopped:(unit -> bool) -> perform_update:(unit -> unit) -> unit
