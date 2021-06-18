open Core
open Async_kernel
open Incr_dom

module Model = struct
  type t = unit

  let cutoff _ _ = true
end

module State = struct
  type t = unit
end

module Action = struct
  type t = Nothing.t [@@deriving sexp]
end

let initial_model = ()
let on_startup ~schedule_action:_ _model = Deferred.unit

let create model ~old_model:_ ~inject:_ =
  let open Incr.Let_syntax in
  let%map model = model
  and time = Incr.Clock.watch_now Incr.clock in
  let apply_action action _ ~schedule_action:_ = Nothing.unreachable_code action in
  let view = time |> Time_ns.to_string_utc |> Vdom.Node.text in
  Component.create ~apply_action model view
;;
