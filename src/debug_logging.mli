open! Core

module Action_logging : sig
  type 'action t
end

module Flags : sig
  type t =
    { should_profile : unit -> bool
    ; should_debug : unit -> bool
    }
end

val init_app
  :  app_id:string
  -> named_logging_filters:(string * ('action -> bool)) list
  -> initial_debug:bool
  -> Flags.t * 'action Action_logging.t

val cleanup_app : app_id:string -> unit

val maybe_log_action
  :  'action Action_logging.t
  -> sexp_of_action:('action -> Sexp.t)
  -> 'action
  -> unit
