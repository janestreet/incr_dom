open! Core_kernel.Std
open Virtual_dom.Std
open Async_kernel.Std
open Js_of_ocaml
open Js_of_ocaml_async
open Common

let document_loaded : unit Deferred.t =
  let ready_state = Js.to_string Dom_html.document##.readyState in
  if String.(ready_state = "complete" || ready_state = "loaded")
  then Deferred.unit
  else
    let loaded = Ivar.create () in
    ignore (
      Dom_html.addEventListener
        Dom_html.document
        (Dom.Event.make "DOMContentLoaded")
        (Dom.handler (fun _ -> Ivar.fill loaded (); Js._true))
        Js._false);
    Ivar.read loaded

let timer_start s =
  Firebug.console##time (Js.string s)
;;

let timer_stop s =
  Firebug.console##timeEnd (Js.string s)
;;

let start
      (type model) (type action)
      (module App : App_intf.S with type Model.t = model and type Action.t = action)
      ~initial_state
      ~on_startup
      ~on_display
  =
  (* This is idempotent and so fine to do. *)
  Async_js.init ();
  don't_wait_for (
    document_loaded >>= fun () ->
    let state = Incr.Var.create initial_state in
    let (r,w) = Pipe.create () in
    let schedule action = Pipe.write_without_pushback w action in
    let view = Incr.observe (App.view (Incr.Var.watch state) ~schedule) in
    Incr.stabilize ();
    let html = ref (Incr.Observer.value view |> ok_exn) in
    let elt = ref (Vdom.Node.to_dom !html :> Dom.element Js.t) in
    Dom_html.document##.body := (Obj.magic !elt);
    on_startup ~schedule state;
    Pipe.iter' r ~f:(fun actions ->
      timer_start "total";
      let old_state = Incr.Var.value state in
      timer_start "apply actions";
      let new_state =
        Queue.fold actions ~init:old_state ~f:(fun state action ->
          (if App.Action.should_log action then
              logf !"Action: %{sexp:App.Action.t}" action);
          App.Action.apply action ~schedule state)
      in
      Incr.Var.set state new_state;
      timer_stop "apply actions";

      timer_start "stabilize";
      Incr.stabilize ();
      timer_stop "stabilize";

      let html' = Incr.Observer.value view |> ok_exn in

      timer_start "diff";
      let patch = Vdom.Node.Patch.create ~previous:!html ~current:html' in
      timer_stop "diff";

      timer_start "patch";
      let elt' = Vdom.Node.Patch.apply patch (!elt :> Dom.element Js.t) in
      timer_stop "patch";

      timer_start "on_display";
      on_display ~schedule old_state new_state;
      timer_stop "on_display";

      html := html';
      elt := elt';
      timer_stop "total";
      Firebug.console##log (Js.string "-------");
      Deferred.unit
    )
  )
