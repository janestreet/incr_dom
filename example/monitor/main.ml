open! Core_kernel.Std
open! Async_kernel.Std
open! Incr_dom
open! Js_of_ocaml

let () =
  let init_loc =
    (* Our deployed version of python SimpleHTTPServer seems to automatically add / at the
       end of search (https://bugs.python.org/issue23112) so we strip it. *)
    let search =
      Js.to_string Dom_html.window##.location##.search
      |> String.rstrip ~drop:(fun c -> c = '/')
    in
    let uri = Ocaml_uri.Uri.of_string search in
    Ocaml_uri.Uri.get_query_param uri "init_loc"
  in
  let monitor = Monitor.create ~name:"My monitor" () in
  let stop = Ivar.create () in
  upon (Ivar.read stop) (fun () ->
    Firebug.console##log (Js.string "Stopped incr_dom app"));
  let all_messages = ref [] in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
    Ivar.fill_if_empty stop ();
    let message = Exn.to_string (Monitor.extract_exn exn) in
    all_messages := message :: !all_messages;
    (* Once the incr_dom app has stopped running, we modify the dom directly *)
    upon (Ivar.read stop) (fun () ->
      let dom =
        let open Vdom in
        Node.body []
          [ Node.h2 [] [ Node.text "Error!" ]
          ; Node.ul []
              (List.rev_map !all_messages ~f:(fun text -> Node.li [] [ Node.text text ]))
          ]
        |> Node.to_dom
      in
      Dom_html.document##.body := dom
    )
  );
  Async_kernel.Scheduler.within ~monitor (fun () ->
    Start_app.simple
      ~stop:(Ivar.read stop)
      ~initial_model:(App.init ?init_loc monitor ~stop)
      (module App)
  )
