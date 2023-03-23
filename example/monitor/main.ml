open! Core
open! Async_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  let init_loc =
    (* Our deployed version of python SimpleHTTPServer seems to automatically add / at the
       end of search (https://bugs.python.org/issue23112) so we strip it. *)
    let search =
      Js.to_string Dom_html.window##.location##.search
      |> String.rstrip ~drop:(Char.equal '/')
    in
    let uri = Uri.of_string search in
    Uri.get_query_param uri "init_loc"
  in
  let monitor = Monitor.create ~name:"My monitor" () in
  let stop = Ivar.create () in
  upon (Ivar.read stop) (fun () ->
    Firebug.console##log (Js.string "Stopped incr_dom app"));
  let all_messages = ref [] in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
    let exn = Monitor.extract_exn exn in
    Ivar.fill_if_empty stop ();
    let js_error = Js_error.of_exn exn in
    (* Raise the exception to the top-level outside of async so that Chrome can print out
       its source-mapped backtrace (and the async program can continue to run). *)
    ignore (Dom_html.setTimeout (fun () -> Option.iter js_error ~f:Js_error.raise_) 0.);
    let backtrace =
      Option.value_map
        js_error
        ~f:(fun js_error ->
          Option.value (Js_error.stack js_error) ~default:"no backtrace found")
        ~default:"no js error found"
    in
    let message = sprintf !"%{Exn} (%s)" exn backtrace in
    all_messages := message :: !all_messages;
    (* Once the incr_dom app has stopped running, we modify the dom directly *)
    let dom =
      let open Vdom in
      Node.body
        [ Node.h2 [ Node.text "Error!" ]
        ; Node.ul (List.rev_map !all_messages ~f:(fun text -> Node.li [ Node.text text ]))
        ]
      |> Node.to_dom
    in
    Dom_html.document##.body := dom);
  Async_kernel_scheduler.within ~monitor (fun () ->
    Start_app.start
      ~stop:(Ivar.read stop)
      ~bind_to_element_with_id:"app"
      ~initial_model:(App.init ?init_loc monitor ~stop)
      (module App))
;;
