open! Core
open Incr_dom
open Js_of_ocaml
open Incr.Let_syntax

module Model = struct
  type t =
    { default_prevented : bool
    ; propagation_stopped : bool
    ; outer_click_count : int
    ; outer_keydown_count : int
    }
  [@@deriving sexp, fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0

  let create () =
    { default_prevented = false
    ; propagation_stopped = false
    ; outer_click_count = 0
    ; outer_keydown_count = 0
    }
  ;;
end

module State = struct
  type t = unit
end

module Action = struct
  type t =
    | Outer_click
    | Outer_keydown
    | Set_default_prevented of bool
    | Set_propagation_stopped of bool
  [@@deriving sexp]

  let should_log _ = false
end

let apply_action (model : Model.t) action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Outer_click -> { model with outer_click_count = model.outer_click_count + 1 }
  | Outer_keydown -> { model with outer_keydown_count = model.outer_keydown_count + 1 }
  | Set_default_prevented default_prevented -> { model with default_prevented }
  | Set_propagation_stopped propagation_stopped -> { model with propagation_stopped }
;;

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let view (m : Model.t Incr.t) ~inject =
  let open Vdom in
  let inner_click =
    match%map m >>| Model.propagation_stopped with
    | true -> Attr.on_click (fun _ -> Event.Stop_propagation)
    | false -> Attr.on_click (fun _ -> Event.Ignore)
  in
  let outer_click = Attr.on_click (fun _ -> inject Action.Outer_click) in
  let outer_keydown = Attr.on_keydown (fun _ -> inject Action.Outer_keydown) in
  let inner_keydown =
    let%map default_prevented = m >>| Model.default_prevented
    and propagation_stopped = m >>| Model.propagation_stopped in
    let events =
      List.filter_opt
        [ Option.some_if default_prevented Event.Prevent_default
        ; Option.some_if propagation_stopped Event.Stop_propagation
        ]
    in
    Attr.on_keydown (fun _ -> Event.Many events)
  in
  let pd_click =
    Attr.on_click (fun evt ->
      (let open Option.Let_syntax in
       let%bind target = Js.Opt.to_option evt##.target in
       let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.input target) in
       inject (Action.Set_default_prevented (Js.to_bool inp##.checked)))
      |> Option.value ~default:Event.Ignore)
  in
  let sp_click =
    Attr.on_click (fun evt ->
      (let open Option.Let_syntax in
       let%bind target = Js.Opt.to_option evt##.target in
       let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.input target) in
       inject (Action.Set_propagation_stopped (Js.to_bool inp##.checked)))
      |> Option.value ~default:Event.Ignore)
  in
  let%map clicks = m >>| Model.outer_click_count
  and keydowns = m >>| Model.outer_keydown_count
  and inner_click = inner_click
  and inner_keydown = inner_keydown in
  Node.body
    [ Node.p
        [ Node.text
            "With prevent default on, typing in the text box will prevent letters from \n\
             showing up. With stop propagation on, keypresses and clicks won't bubble \n\
             to the outer container."
        ]
    ; Node.div
        [ Node.label
            [ Node.text "Prevent default"
            ; Node.input
                ~attr:(Attr.many_without_merge [ Attr.type_ "checkbox"; pd_click ])
                []
            ]
        ; Node.label
            [ Node.text "Stop propagation"
            ; Node.input
                ~attr:(Attr.many_without_merge [ Attr.type_ "checkbox"; sp_click ])
                []
            ]
        ]
    ; Node.div
        ~attr:
          (Attr.many_without_merge [ outer_click; outer_keydown; Attr.id "outer-click" ])
        [ Node.div [ Node.text "Clicks: "; Node.text (Int.to_string clicks) ]
        ; Node.div [ Node.text "Keydowns: "; Node.text (Int.to_string keydowns) ]
        ; Node.div
            ~attr:(Attr.many_without_merge [ inner_click; Attr.id "inner-click" ])
            [ Node.text "Click me inner" ]
        ; Node.input
            ~attr:
              (Attr.many_without_merge [ inner_click; inner_keydown; Attr.type_ "text" ])
            []
        ]
    ]
;;

let create model ~old_model:_ ~inject =
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
;;
