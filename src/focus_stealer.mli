open! Core
open Virtual_dom
open Js_of_ocaml

(** If enabled, the focus stealer prevents the [document.activeElement] from being
    [<body />] (Chrome's default behavior). Instead, it will set focus on the root element
    returned by our app, so that keyboard listeners registered on the app root always
    work. *)

type t

val create : enabled:bool -> t

(** [wrap_root_element] adds the necessary attributes to the root node so that it can
    intercept keyboard events.
    https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/tabindex

    Does not do anything if [enabled] is false. *)
val maybe_wrap_root_element : t -> Vdom.Node.t -> Vdom.Node.t

(** [maybe_potentially_refocus_root_element] checks whether the active element is the
    body, and if so, refocuses the provided app root element.

    Does not do anything if [enabled] is false. *)
val maybe_refocus_root_element : t -> Dom_html.element Js.t -> unit

(** [refocus_on_blur] checks whether focus needs to be transferred back to provided app
    root on every [blur] event.

    Does not do anything if [enabled] is false. *)
val maybe_refocus_on_blur : t -> Dom_html.element Js.t ref -> unit
