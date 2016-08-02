open! Core_kernel.Std
open! Async_kernel.Std
open Js_of_ocaml
open Js_of_ocaml_async
open! Import

let rec chunk_list l n =
  if List.is_empty l then []
  else (
    let (chunk,rest) = List.split_n l n in
    chunk :: chunk_list rest n
  )

let rec refresh_visibility ~schedule (state_var : Entries.Model.t Incr.Var.t) =
  let open Deferred.Let_syntax in
  let entry_ids = Map.keys (Incr.Var.value state_var).entries in
  let%bind () =
    Deferred.List.iter (chunk_list entry_ids 500) ~how:`Sequential ~f:(fun chunk ->
      let with_visibility =
        List.map chunk ~f:(fun entry_id ->
          match
            Js.Opt.to_option
              (Dom_html.document##getElementById (Js.string (Entry_id.id_string entry_id)))
          with
          | None -> (entry_id, false)
          | Some elt -> (entry_id, (Js_misc.element_is_in_viewport elt)))
      in
      let state' =
        List.fold with_visibility ~init:(Incr.Var.value state_var)
          ~f:(fun acc (entry_id,elt_visible) ->
            match Map.find acc.entries entry_id with
            | None -> acc
            | Some entry ->
              if Bool.(=) elt_visible (Entry.Model.is_visible entry) then acc
              else (
                let entries =
                  Map.add acc.entries ~key:entry_id
                    ~data:(Entry.Model.set_visibility entry elt_visible)
                in
                { acc with entries }
              ))
      in
      Incr.Var.set state_var state';
      schedule Entries.Action.nop; (* force a redisplay *)
      Async_js.sleep 0.1
    )
  in
  refresh_visibility ~schedule state_var


let () =
  Start_app.start
    (module Entries)
    ~initial_state:(Entries.example ~entries:1000)
    ~on_startup:(fun ~schedule state ->
      Js_misc.scroll ();
      every (Time_ns.Span.of_sec 1.)  (fun () -> schedule (Entries.Action.kick_n 150));
      every (Time_ns.Span.of_sec 0.5) (fun () -> schedule Entries.Action.nop);
      don't_wait_for @@ refresh_visibility ~schedule state;
    )
    ~project_immutable_summary:(fun model -> Option.map model.focus ~f:fst)
    ~on_display:(fun ~schedule:_ ~old:old_focus model ->
      let new_focus = Option.map model.focus ~f:fst in
      if old_focus <> new_focus then (Js_misc.scroll ())
    )
