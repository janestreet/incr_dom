open! Core_kernel.Std
open! Async_kernel.Std

type key = | Char of Char.t
           | Up
           | Down
           | Left
           | Right
           | Enter
           | Escape
           | Tab
           | Unknown
[@@deriving sexp]

type t = { key: key
         ; shift: bool
         ; alt: bool
         ; meta: bool
         ; ctrl: bool
         }
[@@deriving sexp]

open Js_of_ocaml

let of_event (e:Dom_html.keyboardEvent Js.t) =
  let alt   = Js.to_bool e##.altKey   in
  let meta  = Js.to_bool e##.metaKey  in
  let ctrl  = Js.to_bool e##.ctrlKey  in
  let shift = Js.to_bool e##.shiftKey in
  let key =
    match Js.Optdef.to_option e##.keyIdentifier with
    | None -> Unknown
    | Some s ->
      match Js.to_string s with
      | "Tab"  -> Tab
      | "Esc"  -> Escape
      | "Up"   -> Up
      | "Down" -> Down
      | "Left" -> Left
      | "Right" -> Right
      | "Enter" -> Enter
      | _ ->
        match Option.bind (Js.Optdef.to_option e##.charCode) ~f:Char.of_int with
        | Some c -> Char c
        | None ->
          match Char.of_int e##.keyCode  with
          | Some c -> Char c
          | None -> Unknown
  in
  { key; shift; alt; meta; ctrl }
