open! Core_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml

open Incr.Let_syntax

module Field = struct
  type t =
    | Percentage
    | Speed
    | Aggro
    | Tries
  [@@deriving sexp, compare]

  let to_dom t =
    "edit-" ^ (Sexp.to_string (sexp_of_t t))
end

let parse_percentage s =
  let f = Float.of_string s in
  if 0. <= f && f <= 100. then (
    f
  ) else (
    raise_s [%message "Percentage out of range" (f : float)]
  )

module Model = struct
  type t =
    { percentage : string
    ; speed : string
    ; aggro : string
    ; tries : string
    ; focus : Field.t
    } [@@deriving sexp_of, fields, compare]

  let app old inp parse =
    if String.is_empty inp then (
      old
    ) else (
      parse inp
    )

  let apply t (m : Row.Model.t) =
    try
      Some
        { m with
          percentage = app m.percentage t.percentage parse_percentage
        ; speed      = app m.speed      t.speed      Float.of_string
        ; aggro      = app m.aggro      t.aggro      Float.of_string
        ; tries      = app m.tries      t.tries      Int.of_string
        }
    with _ -> None

  let empty =
    { percentage = ""
    ; speed = ""
    ; aggro = ""
    ; tries = ""
    ; focus = Field.Percentage
    }
end

module Action = struct
  type t =
    | Value of Field.t * string
    | Move_focus of [ `Left | `Right ]
  [@@deriving sexp]

  let should_log _ = false

  let apply action (m : Model.t) =
    match action with
    | Value (Percentage, percentage) -> { m with percentage }
    | Value (Speed     , speed     ) -> { m with speed      }
    | Value (Aggro     , aggro     ) -> { m with aggro      }
    | Value (Tries     , tries     ) -> { m with tries      }
    | Move_focus `Right ->
      { m with
        focus = match m.focus with
          | Percentage -> Speed
          | Speed      -> Aggro
          | Aggro      -> Tries
          | Tries      -> Percentage
      }
    | Move_focus `Left ->
      { m with
        focus = match m.focus with
          | Tries      -> Aggro
          | Aggro      -> Speed
          | Speed      -> Percentage
          | Percentage -> Tries
      }
end

let on_input ~inject field =
  Vdom.Attr.on_input (fun _ value ->
    inject (Action.Value (field, value))
  )

let run_validator validator value =
  if String.is_empty value then (
    true
  ) else (
    try ignore (validator value); true
    with _ -> false
  )

let field ~inject f value validator =
  let open Vdom in
  let%map value = value in
  let invalid = not (run_validator validator value) in
  Node.td []
    [ Node.input
        [ on_input ~inject f
        ; Attr.classes (if invalid then [ "invalid" ] else [])
        ; Attr.create "size" "1"
        ; Attr.style [ "width", "100%" ]
        ; Attr.id (Field.to_dom f)
        ] []
    ]

let edit_id = "edit-row"

let view (m : Model.t Incr.t) ~inject =
  let open Vdom in
  let skip = Node.td [] [] in
  let%map percentage = field ~inject Percentage (m >>| Model.percentage) parse_percentage
  and     speed      = field ~inject Speed      (m >>| Model.speed)      Float.of_string
  and     aggro      = field ~inject Aggro      (m >>| Model.aggro)      Float.of_string
  and     tries      = field ~inject Tries      (m >>| Model.tries)      Int.of_string
  in
  Node.tr ~key:"editable"
    [ Attr.id edit_id ]
    [ skip
    ; skip
    ; skip
    ; skip
    ; percentage
    ; speed
    ; aggro
    ; tries
    ; skip
    ]
