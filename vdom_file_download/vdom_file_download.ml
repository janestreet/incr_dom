open Core
open Virtual_dom

type t =
  { filename : string
  ; mimetype : string
  ; contents : string
  }
[@@deriving fields]

let create = Fields.create
let sexp_of_t t = Sexp.Atom (sprintf "<downloader: %s>" t.filename)

let trigger { filename; mimetype; contents } =
  let open Js_of_ocaml in
  if Ppx_inline_test_lib.am_running
  then
    print_s
      [%message "Download triggered" (filename : string) (mimetype : string) contents]
  else (
    (* https://stackoverflow.com/a/19328891/558592 *)
    let a = Dom_html.createA Dom_html.document in
    let contents_bigstr =
      Typed_array.Bigstring.to_arrayBuffer (Bigstring.of_string contents)
    in
    let blob =
      (* Don't use [blob_from_string]. That has an attractive type but works by first
         converting our OCaml string to a Javascript string, which converts to
         UTF-16. If the string contains random binary data that will distort it. *)
      File.blob_from_any [ `arrayBuffer contents_bigstr ] ~contentType:mimetype
    in
    let url = Dom_html.window##._URL##createObjectURL blob in
    a##setAttribute (Js.string "href") url;
    a##setAttribute (Js.string "download") (Js.string filename);
    a##click;
    Dom_html.window##._URL##revokeObjectURL url)
;;

module Button = struct
  let create
        ?(enabled = true)
        ?(on_click = fun _ -> Vdom.Effect.Ignore)
        ?(extra_attrs = [])
        ~get_download
        ~button_text
        ()
    =
    let open Vdom in
    let trigger_csv_download ev =
      trigger (get_download ());
      on_click ev
    in
    let enabled_disabled =
      if enabled then [] else [ Attr.disabled; Attr.style (Css_gen.color (`Name "grey")) ]
    in
    let attrs =
      [ extra_attrs; enabled_disabled; [ Attr.on_click trigger_csv_download ] ]
      |> List.concat
    in
    Node.button ~attr:(Attr.many_without_merge attrs) [ Node.text button_text ]
  ;;
end
