open! Core_kernel.Std
open Async_kernel.Std
open Incr_dom.Std
open Js_of_ocaml

let the_num_rows = 20_000
let the_num_cols = 50

let cell_id x y = "cell-" ^ Int.to_string x ^ "-" ^ Int.to_string y
let row_id  x   = "row-"  ^ Int.to_string x

module Int_tuple = Tuple.Hashable(Int)(Int)

module Var = Incr.Var

module Model = struct
  module Cell = struct
    type t =
      (* update to this var will not cause rerendering if the row is invisible *)
      { text : string Var.t
      }
      [@@deriving sexp_of, fields]

    let create () = { text = Var.create "0.0000" }
  end

  module Row = struct
    type t =
      { cells          : Cell.t Int.Map.t
      ; focused_column : int option Var.t
      (* Rendering of the whole row will bind on this [visible] ivar, there fore invisible
         rows are very cheap *)
      ; visible        : bool       Var.t
      }
      [@@deriving sexp_of, fields]

    let create () =
      let cells =
        List.map (List.init the_num_cols ~f:Fn.id) ~f:(fun x ->
          x, Cell.create ()
        )
        |> Int.Map.of_alist_exn
      in
      { cells; focused_column = Var.create None; visible = Var.create false }
  end

  type t =
    { rows                 : Row.t Int.Map.t
    ; mutable focused_row  : int
    ; mutable visible_rows : (int * int)
    }
    [@@deriving sexp_of, fields]

  let focus t =
    let col =
      Var.value (Map.find_exn t.rows t.focused_row |> Row.focused_column)
      |> Option.value ~default:0
    in
    t.focused_row, col

  let focus_cell_id t =
    let row, col = focus t in
    cell_id row col

  let t_of_sexp _ = assert false

  let between x ~min ~max = x <= max && x >= min

  let is_row_visible t x =
    let min, max = t.visible_rows in
    between x ~min ~max

  let set_text t x y text =
    let row = Map.find_exn t.rows x in
    let cell = Map.find_exn row.cells y in
    Var.set cell.text text;
    t

  let bound ~min ~max x = Int.min (Int.max x min) max

  let move_focus t dir =
    let move_focused_row diff =
      let row = bound ~min:0 ~max:(the_num_rows - 1) (t.focused_row + diff) in
      if row <> t.focused_row then begin
        let prev_row = Map.find_exn t.rows t.focused_row in
        let new_row  = Map.find_exn t.rows row           in
        let focused_column = Var.value prev_row.focused_column |> Option.value ~default:0 in
        Var.set prev_row.focused_column None;
        Var.set new_row.focused_column (Some focused_column);
        t.focused_row <- row
      end;
      t
    in
    let move_focused_column diff =
      let row = Map.find_exn t.rows t.focused_row in
      let col = Var.value row.focused_column |> Option.value ~default:0 in
      let col = bound ~min:0 ~max:(the_num_cols - 1) (col + diff) in
      Var.set row.focused_column (Some col);
      t
    in
    match dir with
    | `up    -> move_focused_row      1
    | `down  -> move_focused_row    (-1)
    | `left  -> move_focused_column (-1)
    | `right -> move_focused_column   1
  ;;

  let set_visible_rows t from to_ =
    let from = bound ~min:0 ~max:(the_num_rows - 1) from in
    let to_  = bound ~min:0 ~max:(the_num_rows - 1) to_  in
    let (prev_from, prev_to) = t.visible_rows in
    if (prev_from, prev_to) <> (from, to_) then begin
      logf "visible rows %d-%d >= %d-%d" prev_from prev_to from to_;
      for i = prev_from to prev_to do
        if not (between i ~min:from ~max:to_) then begin
          let row = Map.find_exn t.rows i in
          (* when turning a row to be invisible, if the height of rows are not uniform,
             then we can consider remember the "height" fix the height of them. Otherwise
             as we scroll the table height may keep changing. *)
          Var.set row.visible false
        end
      done;
      for i = from to to_ do
        if not (between i ~min:prev_from ~max:prev_to) then begin
          let row = Map.find_exn t.rows i in
          Var.set row.visible true
        end
      done;
    end;
    t.visible_rows <- (from, to_);
    t
end

module Model_summary = struct
  type t = string

  let create = Model.focus_cell_id
end

module Action = struct
  type t =
    | Set_text of int * int * string
    | Move_focus of [`left | `right | `up | `down]
    [@@deriving sexp]

  let apply t ~schedule:_ (m:Model.t) =
    match t with
    | Set_text         (x, y, text) -> Model.set_text         m x y text
    | Move_focus       dir          -> Model.move_focus       m dir

  let random () =
    Set_text (Random.int the_num_rows
             ,Random.int the_num_cols
             ,Random.float 1. |> Float.to_string_hum ~decimals:4)

  let should_log _ = false
end

let init () =
  let rows =
    List.init the_num_rows ~f:(fun x ->
      let row = Model.Row.create () in
      if x = 0 then begin
        Var.set row.visible true;
        Var.set row.focused_column (Some 0)
      end;
      x, row
    )
    |> Int.Map.of_alist_exn
  in
  { Model.rows
  ; focused_row = 0
  ; visible_rows = (0, 0)
  }

let on_startup ~schedule _ =
  Clock_ns.every (Time_ns.Span.of_sec 0.05) (fun () ->
    for _ = 1 to 250 do
      schedule (Action.random ())
    done
  )

let text_td text = Vdom.Node.td [] [Vdom.Node.text text]

let invisible_cell x y ~focused =
  let id = Vdom.Attr.id (cell_id x y) in
  let attr =
    if focused then [ Vdom.Attr.create "bgcolor" "#FFCC00" ; id ] else [ id ]
  in
  Some (Vdom.Node.td attr [Vdom.Node.text "---"])

let view (m : Model.t Incr.t) ~schedule ~viewport_changed:_ =
  let open Incr.Let_syntax in
  let on_keypress =
    Vdom.Attr.on_keypress (fun ev ->
      let kp = Keypress.of_event ev in
      match kp.key with
      | Char 'j' -> schedule (Action.Move_focus `up)
      | Char 'k' -> schedule (Action.Move_focus `down)
      | Char 'h' -> schedule (Action.Move_focus `left)
      | Char 'l' -> schedule (Action.Move_focus `right)
      | _ -> ()
    )
  in
  let%map cells =
    Incr.Map.filter_mapi' (m >>| Model.rows)
      ~f:(fun ~key:row_index ~data:(row:Model.Row.t Incr.t) ->
        let%bind is_visible =
          let%bind row = row in
          Var.watch row.visible
        in
        if not is_visible then (
          (* for invisible rows, we

             - create a constant node, which never ticks unless row becomes visible again
             - only use a single simple "td" as space holder *)
          let td =
            let id = Vdom.Attr.id (cell_id row_index 0) in
            let colspan = Vdom.Attr.create "colspan" (Int.to_string the_num_cols) in
            Vdom.Node.td [colspan; id] [ Vdom.Node.text "-" ]
          in
          return (Some (Vdom.Node.tr [ Vdom.Attr.id (row_id row_index) ] [ td ])))
        else (
          let focused_col =
            let%bind row = row in
            Var.watch row.focused_column
          in
          let%map row_cells =
            Incr.Map.filter_mapi' (row >>| Model.Row.cells)
              ~f:(fun ~key:col_index ~data:cell ->
                let text =
                  let%bind cell = cell in
                  Var.watch cell.text
                in
                (* create a new bool node [is_focused] here instead of depending on
                   [focused_col] directly for better cutoff behavior *)
                let is_focused =
                  match%map focused_col with
                  | None -> false
                  | Some x -> x = col_index
                in
                Incr.map2 text is_focused ~f:(fun text is_focused ->
                  let attrs =
                    let id = Vdom.Attr.id (cell_id row_index col_index) in
                    if is_focused then [ Vdom.Attr.create "bgcolor" "#FFCC00" ; id ]
                    else [ id ]
                  in
                  Some (Vdom.Node.td attrs [Vdom.Node.text text])
                )
              )
          in
          let row_index_cell = text_td (Int.to_string_hum row_index) in
          Some (Vdom.Node.tr
                  [ Vdom.Attr.id (row_id row_index) ]
                  (row_index_cell :: Map.data row_cells))
        ))
  in
  Vdom.Node.body [on_keypress] [Vdom.Node.table [] (Map.data cells)]
;;

let visible_rows_of_model (_:Model.t) =
  let nth_element_id n = row_id n in
  Js_misc.find_visible_range ~length:the_num_rows ~nth_element_id Js_misc.Rows
;;

let update_visibility m =
  match visible_rows_of_model m with
  | None             -> m
  | Some (from, to_) ->
    (* cover half a page above and below current page *)
    let from, to_ =
      let length = (to_ + 1 - from) / 2 in
      let from   = from - length        in
      let to_    = to_ + length         in
      from, to_
    in
    Model.set_visible_rows m from to_

let on_display ~schedule:_ ~old:old_id model =
  let new_id = Model.focus_cell_id model in
  if old_id <> new_id then begin
    match Js.Opt.to_option (Dom_html.document##getElementById (Js.string new_id)) with
    | None     -> ()
    | Some elt ->
      if not (Js_misc.element_is_in_viewport elt) then begin
        logf "scroll to %s" new_id;
        Js_misc.scroll ~id:new_id ()
      end
  end

