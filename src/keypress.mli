open! Core_kernel.Std
open! Async_kernel.Std
open Js_of_ocaml

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

val of_event : Dom_html.keyboardEvent Js.t -> t
