open! Core
open Js_of_ocaml

(** The Js_of_ocaml type Dom_html.element doesn't have the correct options for their
    `focus` method. Cast to this in order to work around this bug. *)
type focusable =
  < focus : < preventScroll : bool Js.t Js.readonly_prop > Js.t -> unit Js.meth >

let as_focusable : Dom_html.element Js.t -> focusable Js.t = Js.Unsafe.coerce

let potentially_refocus_root_element element =
  let refocus_root_element () =
    (* If the element to focus is an element, cast it into the
         more permissive "focusable" type defined at the top of
         this file, and then focus that. *)
    Dom_html.CoerceTo.element element
    |> Js.Opt.to_option
    |> Option.map ~f:as_focusable
    |> Option.iter ~f:(fun element ->
      element##focus
        (object%js
           val preventScroll = Js._true
        end))
  in
  (* [Js_of_ocaml.Dom_html.document] has the wrong type.
       It is listed as [Dom_html.element Js.Opt.t Js.t] but should be either

       - [Dom_html.element Js.Optdef.t Js.t] or
       - [Dom_html.element Js.Opt.t Js.Optdef.t Js.t].

       Fortunately, we can "correct" the type by promoting it
       into an [Optdef.t] and then immediately casting back down to
       an option.

       This sequence of calls produces this javascript code:
       {v
         var focus_lost;
         var active;

         var documentActiveElement = Dom_html.document##.activeElement;
         if (documentActiveElement !== undefined) {
             active = documentActiveElement;
         } else {
             active = null;
         }

         if (active === null) {
             focus_lost = true;
         } else {
             focus_lost = active.tagName == "body";
         }

         if (focus_lost && document.hasFocus()) {
           refocus_root_element()
         }
       v} *)
  let active =
    Js.Optdef.get
      (Js.Optdef.return Dom_html.document##.activeElement)
      (fun () -> Js.Opt.empty)
  in
  let focus_lost =
    Js.Opt.case
      active
      (* refocus if there is no active element. This never seems to happen in Chrome
           as of v124, but might be the case in other browsers. *)
        (fun () -> true)
      (* refocus if the active element is <body> *)
        (fun active_elt -> Js.Opt.test (Dom_html.CoerceTo.body active_elt))
  in
  (* If we are in an iframe, we don't want to steal focus unless we have focus. *)
  let has_focus =
    let document : < hasFocus : bool Js.t Js.meth > Js.t =
      Js_of_ocaml.Js.Unsafe.coerce Dom_html.document
    in
    Js_of_ocaml.Js.to_bool document##hasFocus
  in
  if focus_lost && has_focus then refocus_root_element ()
;;

(* As of Chrome v124, [blur] runs both when the [activeElement] changes,
   and when focus moves to another tab / window:
   - In the former case, a [relatedTarget] of [null] means that focus will move to
   [<body />].
   - In the latter case, [blur] is dispatched twice; with [relatedTarget] being
   [null] and [undefined] respectively, but [activeElement] doesn't change.

   That's why it's critical that in [potentially_refocus_root_element], we check
   that [activeElement] is on the [<body />] or [null],
   AND that [document.hasFocus()] returns [true].

   We run this on [capture] because [blur] doesn't bubble.
*)
let refocus_on_blur element_ref =
  Dom.addEventListenerWithOptions
    Dom_html.window
    Dom_html.Event.blur
    ~capture:Js._true
    (Dom_html.handler (fun e ->
       (* [Js.Unsafe.*] is like [Obj.magic]. We should be explicit about what we expect. *)
       let e : < relatedTarget : Dom_html.element Js.t Js.opt Js.readonly_prop > Js.t =
         Js.Unsafe.coerce e
       in
       let receiving_focus = e##.relatedTarget in
       if not (Js.Opt.test receiving_focus)
       then potentially_refocus_root_element !element_ref;
       Js._true))
  |> (ignore : Dom_html.event_listener_id -> unit)
;;

let wrap_root_element root =
  let open Virtual_dom.Vdom in
  match (root : Node.t) with
  | Element element ->
    let focus_attrs =
      let should_add_focus_modifiers =
        element
        |> Node.Element.attrs
        |> Attr.Expert.contains_name "disable_tab_index"
        |> not
      in
      match should_add_focus_modifiers with
      | true -> Attr.(style (Css_gen.outline ~style:`None ()) @ tabindex 0)
      | false -> Attr.empty
    in
    let add_new_attrs attrs = Attr.(focus_attrs @ attrs) in
    element |> Node.Element.map_attrs ~f:add_new_attrs |> Node.Element
  | _ -> root
;;

module Enabled_status = struct
  type t =
    | Enabled
    | Disabled
end

type t = { enabled_status : Enabled_status.t } [@@unboxed]

let create ~enabled =
  let enabled_status : Enabled_status.t = if enabled then Enabled else Disabled in
  { enabled_status }
;;

let maybe_refocus_on_blur t element_ref =
  match t.enabled_status with
  | Enabled -> refocus_on_blur element_ref
  | Disabled -> ()
;;

let maybe_refocus_root_element t element =
  match t.enabled_status with
  | Enabled -> potentially_refocus_root_element element
  | Disabled -> ()
;;

let maybe_wrap_root_element t element =
  match t.enabled_status with
  | Enabled -> wrap_root_element element
  | Disabled -> element
;;
