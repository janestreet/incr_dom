open! Core
open! Async_kernel
open Incr_dom

module Model = struct
  type t = unit

  let cutoff = phys_equal
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
  let%map model = model in
  let apply_action action _ ~schedule_action:_ = Nothing.unreachable_code action in
  let view = Vdom.Node.text "hello world" in
  Component.create ~apply_action model view
;;
