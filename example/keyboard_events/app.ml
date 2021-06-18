open Core
open Async_kernel
open Incr_dom
module Js = Js_of_ocaml.Js

module Model = struct
  type t =
    { last_key_code : int
    ; text_box_content : string
    }
  [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

module State = struct
  type t = unit
end

module Action = struct
  type t =
    | Set_key_code of int
    | Set_text_content of string
  [@@deriving sexp_of]
end

let initial_model = { Model.last_key_code = 0; text_box_content = "type here" }
let on_startup ~schedule_action:_ _model = Deferred.unit

let create (model : Model.t Incr.t) ~old_model:_ ~(inject : Action.t -> Vdom.Event.t) =
  let open Incr.Let_syntax in
  let open Vdom in
  let%map apply_action =
    let%map model = model in
    fun action _ ~schedule_action:_ ->
      match (action : Action.t) with
      | Set_key_code k -> { model with last_key_code = k }
      | Set_text_content s -> { model with text_box_content = s }
  and view =
    let%map model = model in
    let style =
      let ( @> ) = Css_gen.( @> ) in
      Css_gen.width (`Vw (Percent.of_percentage 50.))
      @> Css_gen.height (`Vh (Percent.of_percentage 50.))
      @> Css_gen.font_size (`Px 30)
      @> Css_gen.padding ~top:(`Px 30) ~left:(`Px 30) ()
      @> Css_gen.color (`Name "white")
      @> Css_gen.background_color (`Name "red")
    in
    let container_attributes =
      [ Attr.style style; Attr.on_keypress (fun e -> inject (Set_key_code e##.keyCode)) ]
    in
    let contents =
      [ Node.text (model.last_key_code |> Int.to_string)
      ; Node.textarea
          ~attr:
            (Attr.many_without_merge
               [ Attr.string_property "value" model.text_box_content
               ; Attr.on_keypress (fun _ -> Event.Stop_propagation)
               ; Attr.on_keyup (fun e ->
                   match
                     let open Option.Let_syntax in
                     let%map target =
                       Js.Opt.to_option
                         (Js.Opt.bind e##.target Js_of_ocaml.Dom_html.CoerceTo.textarea)
                     in
                     Js.to_string target##.value
                   with
                   | Some s ->
                     Event.Many [ Event.Stop_propagation; inject (Set_text_content s) ]
                   | None -> Event.Stop_propagation)
               ])
          []
      ]
    in
    if Int.rem model.last_key_code 2 = 0
    then Node.div ~attr:(Attr.many_without_merge container_attributes) contents
    else Node.span ~attr:(Attr.many_without_merge container_attributes) contents
  and model = model in
  (* Note that we don't include [on_display] or [update_visibility], since
     these are optional arguments *)
  Component.create ~apply_action model view
;;
