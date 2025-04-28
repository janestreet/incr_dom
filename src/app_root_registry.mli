open! Core
open Js_of_ocaml

type handle

val register_app_root : Dom_html.element Js.t ref -> handle
val unregister_app_root : handle -> unit
val connected_app_roots : unit -> Dom_html.element Js.t list
