open Core
open Async_kernel
open Incr_dom

module Model = struct
  type t = { counter : int } [@@deriving fields, compare, sexp]

  let cutoff = [%compare.equal: t]
end

module State = struct
  type t = unit
end

module Action = struct
  type t = Increment [@@deriving sexp_of]
end

let initial_model = { Model.counter = 0 }

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Action.Increment);
  Deferred.unit
;;

let create model ~old_model:_ ~inject:_ =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map counter = model >>| Model.counter in
    fun (Increment : Action.t) _ ~schedule_action:_ -> { Model.counter = counter + 1 }
  and view =
    let%map counter =
      let%map counter = model >>| Model.counter in
      Vdom.Node.div [ Vdom.Node.text (Int.to_string counter) ]
    in
    Vdom.Node.body [ counter ]
  and model = model in
  (* Note that we don't include [on_display] or [update_visibility], since
     these are optional arguments *)
  Component.create ~apply_action model view
;;
