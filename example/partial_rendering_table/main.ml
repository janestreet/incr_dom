open! Core
open! Js_of_ocaml
open! Incr_dom

let () =
  let count =
    let hash = Dom_html.window##.location##.search |> Js.to_string in
    (* Our deployed version of python SimpleHTTPServer seems to automatically add / at the
       end of search (https://bugs.python.org/issue23112) so we strip it. *)
    let hash =
      String.strip
        ~drop:(function
          | '/' | '?' -> true
          | _ -> false)
        hash
    in
    try Some (Int.of_string hash) with
    | _ -> None
  in
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:(App.Model.create (Option.value ~default:50000 count))
;;
