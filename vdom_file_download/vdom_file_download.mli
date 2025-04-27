(** A utility for causing the browser to download a file with the contents taken from a
    string. *)

open! Core

type t [@@deriving sexp_of]

val create : filename:string -> mimetype:string -> contents:string -> t

(** Immediately cause the browse to trigger a download for [t]. You can use this to build
    a more complex UI for downloads than is offered by [Button.create] -- e.g. a button
    that calls an RPC and downloads a file based on the result.

    In tests, this just prints a message rather than triggering the download. *)
val trigger : t -> unit

module Button : sig
  open Js_of_ocaml
  open Virtual_dom.Vdom

  (** Creates a button which, when clicked, will create a download using [get_download]
      and then trigger it. *)
  val create
    :  ?enabled:bool
    -> ?on_click:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t)
         (** What event, aside from triggering the download, should happen when the button
             is clicked? *)
    -> ?extra_attrs:Attr.t list
    -> get_download:(unit -> t)
    -> button_text:string
    -> unit
    -> Node.t
end
