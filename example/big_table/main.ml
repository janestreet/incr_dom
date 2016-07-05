open! Core_kernel.Std
open Async_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml

module Model = Table.Model

let visible_rows_of_model (_:Model.t) =
  let nth_element_id n = Table.row_id n in
  Js_misc.find_visible_range ~length:Table.the_num_rows ~nth_element_id Js_misc.Rows
;;

let () =
  Start_app.start
    (module Table)
    ~initial_state:(Table.init ())
    ~on_startup:(fun ~schedule m_var ->
      Clock_ns.every (Time_ns.Span.of_sec 0.05) (fun () ->
        for _ = 1 to 250 do
          schedule (Table.Action.random ())
        done
      );
      begin
        Clock_ns.every (Time_ns.Span.of_sec 0.1) (fun () ->
          match visible_rows_of_model (Incr.Var.value m_var) with
          | None             ->          ()
          | Some (from, to_) ->
            (* cover half a page above and below current page *)
            let from, to_ =
              let length = (to_ + 1 - from) / 2 in
              let from   = from - length        in
              let to_    = to_ + length         in
              from, to_
            in
            schedule (Table.Action.Set_visible_rows (from, to_))
        )
      end
    )
    ~on_display:(fun ~schedule:_ _old_model (new_model:Model.t) ->
      let id = Model.focus_cell_id new_model in
      match Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) with
      | None     -> ()
      | Some elt ->
        if not (Js_misc.element_is_in_viewport elt) then begin
          logf "scroll to %s" id;
          Js_misc.scroll ~id ()
        end
    )
