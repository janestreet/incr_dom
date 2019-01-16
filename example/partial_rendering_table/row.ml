open! Core_kernel
open! Incr_dom
open Incr.Let_syntax
module Id : Identifiable = String

module Model = struct
  type t =
    { data : string
    ; font_size : int
    }
  [@@deriving sexp_of, fields, compare]

  let create ~data = { data; font_size = 16 }
end

module Action = struct
  type t =
    | Change_data of string
    | Increase_font
    | Increase_font_by of int
  [@@deriving sexp_of]

  let apply action (model : Model.t) =
    match action with
    | Change_data data -> { model with data }
    | Increase_font -> { model with font_size = model.font_size + 3 }
    | Increase_font_by size -> { model with font_size = model.font_size + size }
  ;;

  let should_log action =
    match action with
    | Change_data _ -> true
    | Increase_font -> true
    | Increase_font_by _ -> true
  ;;
end

let view (model : Model.t Incr.t) ~row_id ~inject =
  let onclick = Vdom.Attr.on_click (fun _ -> inject Action.Increase_font) in
  let%map model = model in
  Vdom.Node.div
    ~key:(Id.to_string row_id)
    [ Vdom.Attr.style Css_gen.(font_size (`Px model.font_size))
    ; Vdom.Attr.id (Id.to_string row_id)
    ; onclick
    ]
    [ Vdom.Node.text model.data ]
;;
