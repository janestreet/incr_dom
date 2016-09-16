open! Core_kernel.Std
open Incr_dom.Std

module Model = struct
  type t = {
    counters : int Int.Map.t
  } [@@deriving sexp, fields, compare]

  let add_new t =
    let counters = Int.Map.add t.counters ~key:(Map.length t.counters) ~data:0 in
    { counters }

  (* no bounds checks *)
  let update t pos diff =
    let old_val = Map.find_exn t.counters pos in
    let counters = Int.Map.add t.counters ~key:pos ~data:(old_val + diff) in
    { counters }

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | New_counter
    | Update of int * int (* pos, diff *)
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action action model _state =
  match (action:Action.t) with
  | New_counter -> Model.add_new model
  | Update (pos, diff) -> Model.update model pos diff

let update_visibility m = m

let on_startup ~schedule:_ _ =
  Async_kernel.Std.return ()

let on_display ~old:_ _ _ = ()

let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let on_add_new_click = Attr.on_click (fun _ev -> inject Action.New_counter) in
  let add_new_counter_button =
    Node.div []
      [ Node.button [on_add_new_click] [Node.text "add new counter"] ]
  in
  let button txt pos diff =
    let on_click _ev = inject (Action.Update (pos, diff)) in
    Vdom.Node.button
      [ Attr.on_click on_click ]
      [ Node.text txt ]
  in
  let%map elements =
    Incr.Map.filter_mapi' (m >>| Model.counters) ~f:(fun ~key:pos ~data:value ->
      let button_minus = button "-" pos (-1) in
      let button_plus = button "+" pos 1 in
      let%map value = value in
      Some (Node.div []
              [ button_minus; Node.text (Int.to_string value); button_plus ]))
  in
  Node.body [] (add_new_counter_button :: (Map.data elements))
