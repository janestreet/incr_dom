open! Core_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml

open Incr.Let_syntax

module Id = struct
  include Int

  let to_dom x = "row-" ^ (Int.to_string x)
end

module Model = struct
  type t =
    { percentage : float
    ; speed : float
    ; aggro : float
    ; tries : int
    ; owner : string
    ; updates : int
    ; price : float
    ; position : int

    ; height : int
    } [@@deriving sexp_of, compare]

  let empty =
    { percentage = 0.
    ; speed = 0.
    ; aggro = 0.
    ; tries = 0
    ; owner = ""
    ; updates = 0
    ; price = 0.
    ; position = 0

    ; height = 18
    }
end

module Action = struct
  type t =
    | Set_price of float
    | Set_position of int
    | Change_position of int
    | Set_owner of string
    [@@deriving sexp]

  let apply action (m : Model.t) =
    let updates = m.updates + 1 in
    match action with
    | Set_price price -> { m with price; updates }
    | Set_position position -> { m with position; updates }
    | Change_position delta -> { m with position = m.position + delta; updates }
    | Set_owner owner -> { m with owner; updates }

  let should_log = function
    | Set_price _ | Set_position _ | Change_position _ -> false
    | Set_owner _ -> true
end

let column_names =
  [ "ID"
  ; "Owner"
  ; "Price"
  ; "Position"
  ; "Percentage"
  ; "Speed"
  ; "Aggression"
  ; "Tries"
  ; "Updates"
  ]

let header ?widths attrs =
  let open Vdom in
  let widths =
    match widths with
    | Some x -> x
    | None ->
      Incr.const (List.init (List.length column_names) ~f:(fun _ -> None))
  in
  let%map widths = widths and attrs = attrs in
  let children =
    List.map2_exn column_names widths ~f:(fun column_name width ->
      let attrs =
        match width with
        | None -> []
        | Some width -> [ Attr.style_css (sprintf "width: %dpx" width) ]
      in
      Node.th attrs [ Node.text column_name ])
  in
  Node.tr attrs children

let view ~id (m : Model.t Incr.t) ~focused ~focus_me =
  let open Vdom in
  let onclick = Attr.on_click (fun _ -> focus_me) in
  let key = Id.to_dom id in
  let%map m = m and focused = focused in
  Node.tr ~key
    [ Attr.id key
    ; onclick
    ; Attr.classes (if focused then [ "row-focused" ] else [])
    ]
    (Node.th [] [ Node.text (Id.to_string id) ]
     :: List.map ~f:(fun text -> Node.td [] [ Node.text text ])
          [ m.owner
          ; sprintf !"$%.2f" m.price
          ; Int.to_string m.position
          ; sprintf !"%.2f%%" m.percentage
          ; Float.to_string m.speed
          ; Float.to_string m.aggro
          ; Int.to_string m.tries
          ; Int.to_string m.updates
          ])
