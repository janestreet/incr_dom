open! Core_kernel.Std
open! Async_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml

let () =
  let init_loc =
    (* Our deployed version of python SimpleHTTPServer seems to automatically add / at the
       end of search (https://bugs.python.org/issue23112) so we strip it. *)
    Dom_html.window##.location##.search
    |> Js.to_string
    |> String.strip ~drop:(fun c -> c = '/' || c = '?')
  in
  let monitor = Monitor.create ~name:"My monitor" () in
  Async_kernel.Scheduler.within ~monitor (fun () ->
    Start_app.simple
      ~initial_model:(App.init init_loc)
      (module App)
  );
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
    Firebug.console##error (Js.string (Exn.to_string exn))
  )
