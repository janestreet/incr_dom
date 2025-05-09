open! Core
open Js_of_ocaml

type handle = int

let global_handle = ref 0
let global_app_roots = Hashtbl.create (module Int)

let next_handle () =
  incr global_handle;
  !global_handle
;;

let register_app_root element_ref =
  let handle = next_handle () in
  (* [add_exn] is ok because we obtain a separate handle for each app root. *)
  Hashtbl.add_exn global_app_roots ~key:handle ~data:(handle, element_ref);
  handle
;;

let unregister_app_root handle = Hashtbl.remove global_app_roots handle

let connected_app_roots () =
  Hashtbl.data global_app_roots
  |> List.filter_map ~f:(fun (handle, element_ref) ->
    match Js.Opt.to_option (Dom_html.CoerceTo.element !element_ref) with
    | None -> None
    | Some element ->
      let element' : < isConnected : bool Js.t Js.readonly_prop > Js.t =
        Js.Unsafe.coerce element
      in
      if Js.to_bool element'##.isConnected
      then Some element
      else (
        print_s
          [%message
            "BUG! There should never be a disconnected app root. Perhaps you forgot to \
             call [Handle.stop] on your Bonsai app handle?"];
        Console.console##error_2 (Js.string "Disconnected app root: ") element;
        unregister_app_root handle;
        None))
;;
