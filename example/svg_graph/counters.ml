open! Core_kernel
open Incr_dom
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr

let maximum_data_points = 10
let minimum_data_value = 0
let maximum_data_value = 10

module Model = struct
  type t = { counters : int Int.Map.t } [@@deriving sexp, fields, equal]

  let add_new t =
    let map_len = Map.length t.counters in
    print_s [%message (map_len : int)];
    if map_len >= maximum_data_points
    then t
    else { counters = Map.set t.counters ~key:map_len ~data:1 }
  ;;

  let update t ~pos ~diff =
    match Map.find t.counters pos with
    | None -> t
    | Some old_val ->
      let new_val = old_val + diff in
      if new_val < minimum_data_value || new_val >= maximum_data_value
      then t
      else { counters = Map.set t.counters ~key:pos ~data:(old_val + diff) }
  ;;

  let cutoff t1 t2 = equal t1 t2
end

module Action = struct
  type t =
    | New_counter
    | Update of
        { pos : int
        ; diff : int
        }
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | New_counter -> Model.add_new model
  | Update { pos; diff } -> Model.update model ~pos ~diff
;;

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let on_add_new_click = Attr.on_click (fun _ev -> inject New_counter) in
  let add_new_counter_button =
    Node.div [] [ Node.button [ on_add_new_click ] [ Node.text "add new counter" ] ]
  in
  let button txt ~pos ~diff =
    let on_click = Attr.on_click (fun _ -> inject (Update { pos; diff })) in
    let open Css_gen in
    let button_style = min_width (`Px 0) @> margin_top (`Px 3) in
    Node.button [ on_click; Attr.style button_style ] [ Node.text txt ]
  in
  let elements =
    Map.mapi m.counters ~f:(fun ~key:pos ~data:value ->
      let button_minus = button "-" ~pos ~diff:(-1) in
      let button_plus = button "+" ~pos ~diff:1 in
      let text_span =
        Node.span
          [ Attr.style @@ Css_gen.padding ~left:(`Px 10) ~right:(`Px 10) () ]
          [ Node.text (Int.to_string value) ]
      in
      Node.div [] [ button_minus; text_span; button_plus ])
  in
  let points =
    Map.to_alist m.counters
    |> List.map ~f:(Tuple2.map_snd ~f:(fun v -> maximum_data_value - v))
  in
  let points =
    [ [ 0, maximum_data_value ]
    ; points
    ; [ Map.length m.counters - 1, maximum_data_value ]
    ]
    |> List.concat
    |> List.map ~f:(fun (a, b) -> Float.of_int a, Float.of_int b)
  in
  let graph =
    Node_svg.svg
      [ Attr_svg.width 500.0
      ; Attr_svg.height 500.0
      ; Attr_svg.viewbox
          ~min_x:0.0
          ~min_y:0.0
          ~width:(Float.of_int maximum_data_points)
          ~height:(Float.of_int maximum_data_value)
      ]
      [ Node_svg.linear_gradient
          [ Attr.id "my-gradient"
          ; Attr_svg.(gradient_transform [ Rotate { a = `Deg 90.0; x = 0.0; y = 0.0 } ])
          ]
          [ Node_svg.stop
              [ Attr_svg.offset Percent.zero; Attr_svg.stop_color (`Hex "#254E70") ]
              []
          ; Node_svg.stop
              [ Attr_svg.offset (Percent.of_mult 0.5)
              ; Attr_svg.stop_color (`Hex "#37718E")
              ]
              []
          ; Node_svg.stop
              [ Attr_svg.offset (Percent.of_mult 1.0)
              ; Attr_svg.stop_color (`Hex "#8EE3EF")
              ]
              []
          ]
      ; Node_svg.polyline
          [ Attr.style
              (Css_gen.create ~field:"vector-effect" ~value:"non-scaling-stroke")
          ; Attr_svg.points points
          ; Attr_svg.fill (`Url "#my-gradient")
          ; Attr_svg.stroke (`Name "black")
          ]
          []
      ]
  in
  Node.div
    [ Attr.style @@ Css_gen.padding_left @@ `Px 10 ]
    (Jane_web_style.Css.style_4 :: graph :: add_new_counter_button :: Map.data elements)
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view
;;
