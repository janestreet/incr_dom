open! Core_kernel
open! Import

module Model = struct
  type edit = { column: string; value: string }
  [@@deriving compare, sexp]

  type edit_state =
    | Not_editing
    | Editing of edit list
  [@@deriving compare, sexp]

  type t =
    { rows: Row.Model.t Row_id.Map.t
    ; pattern: string
    ; edit_state: edit_state
    ; table: Ts_table.Model.t
    }
  [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0

  let set_pattern t pattern = { t with pattern = String.lowercase pattern }
end

module Model_summary = struct
  type t = Model.t
  let create (m:Model.t) _ = m
end

module Derived_model = struct
  type t =
    { table: Row.Model.t Ts_table.Derived_model.t }
  [@@deriving fields]

  let create (m:Model.t Incr.t) =
    let columns = List.map ~f:Column.to_table_widget_column Row.Model.columns in
    let columns = Incr.const (List.mapi columns ~f:(fun i col -> (i, col))) in
    let rows =
      let%bind pattern = m >>| Model.pattern in
      Incr.Map.filter_mapi (m >>| Model.rows) ~f:(fun ~key:_ ~data ->
        Option.some_if (Row.Model.matches_pattern data pattern) data)
    in
    let table_model = m >>| Model.table in
    let%map table = Ts_table.Derived_model.create table_model ~rows ~columns in
    { table }
end

module Action = struct
  type t =
    | Escape
    | Set_pattern of string
    | Table_action of Ts_table.Action.t
    | Row_action of Row_id.t * Row.Action.t
    | Big_kick of Row_id.t array sexp_opaque
    | Edit_start
    | Remember_edit of Model.edit
    | Commit_edits
    | Add_focused_sort_col
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = { schedule : Action.t -> unit }
  [@@deriving fields]
end

let apply_row_action (m:Model.t) row_id (action:Row.Action.t) =
  let rows = Map.change m.rows row_id ~f:(function
    | None -> None
    | Some row ->
      Some (Row.apply_action action row))
  in
  { m with rows }
;;

let remember_edit (m:Model.t) (new_edit:Model.edit) =
  match m.edit_state with
  | Not_editing -> m
  | Editing edits ->
    let edits = List.filter edits ~f:(fun edit -> edit.column <> new_edit.column) in
    { m with edit_state = Editing (new_edit :: edits) }
;;

let commit_edits (m:Model.t) =
  let m =
    match m.edit_state with
    | Not_editing -> m
    | Editing edits ->
      match Ts_table.Model.focus_row m.table with
      | None -> m
      | Some row_id ->
        let rows = Map.change m.rows row_id ~f:(function
          | None -> None
          | Some row ->
            Some (List.fold edits ~init:row ~f:(fun r e ->
                     Row.Model.apply_edit r ~column:e.column e.value)))
        in
        { m with rows }
  in
  { m with edit_state = Not_editing }

let add_focused_sort_col (m:Model.t) =
  match Ts_table.Model.focus_col m.table with
  | None        -> m
  | Some col_id ->
    let table =
      Ts_table.Model.cycle_sorting m.table col_id ~next_dir:Ts_table.Sort_dir.next
        ~keep_existing_cols:()
    in
    { m with table }

let escape (m:Model.t) (d:Derived_model.t) =
  match m.edit_state with
  | Not_editing ->
    let action = Ts_table.Action.set_focus_row None in
    { m with table = (Ts_table.apply_action m.table d.table action) }
  | Editing _ -> { m with edit_state = Not_editing }

let big_kick (m:Model.t) ids =
  let num = Array.length ids / 120 in
  Sequence.fold (Sequence.range 1 num) ~init:m ~f:(fun m i ->
    let id = ids.(Random.int (Array.length ids)) in
    let m = apply_row_action m id Row.Action.kick_price in
    if i mod 3 <> 0 then m
    else (apply_row_action m id Row.Action.kick_position)
  )

let apply_action
      (action:Action.t) (m:Model.t) _
      ~(recompute_derived : Model.t -> Derived_model.t)
  =
  let d = lazy (recompute_derived m) in
  match action with
  | Escape                -> escape m (force d)
  | Set_pattern s         -> Model.set_pattern m s
  | Table_action a        ->
    {m with table = (Ts_table.apply_action m.table (force d).table a)}
  | Row_action (id,x)     -> apply_row_action m id x
  | Big_kick ids          -> big_kick m ids
  | Edit_start            -> { m with edit_state = Editing [] }
  | Remember_edit edit    -> remember_edit m edit
  | Commit_edits          -> commit_edits m
  | Add_focused_sort_col  -> add_focused_sort_col m

let search_input_id = "search-input"

let update_visibility (m:Model.t) (d:Derived_model.t) ~recompute_derived:_ =
  let table = Ts_table.update_visibility m.table d.table in
  {m with table}

let key_handler ~inject =
  let open Vdom in
  Attr.on_keydown (fun ev ->
    let target_elem = Js.Opt.to_option ev##.target in
    let target_id =
      Option.map target_elem ~f:(fun elem -> Js.to_string elem##.id)
    in
    let is_input =
      Option.value_map target_elem ~f:(fun elem ->
        Option.is_some (Js.Opt.to_option (Dom_html.CoerceTo.input elem))) ~default:false
    in
    let is_search_input =
      Option.value_map target_id ~f:(String.equal search_input_id) ~default:false
    in
    let ignore_if cond event = if cond then Event.Ignore else event in
    let move_focus_row dir =
      Event.Many [
        inject (Action.Table_action (Ts_table.Action.move_focus_row dir));
        Event.Prevent_default ]
    in
    let move_focus_col dir =
      Event.Many [
        inject (Action.Table_action (Ts_table.Action.move_focus_col dir));
        Event.Prevent_default ]
    in
    let page_focus_row dir =
      Event.Many [
        inject (Action.Table_action (Ts_table.Action.page_focus_row dir));
        Event.Prevent_default ]
    in
    if Js.to_bool ev##.ctrlKey || Js.to_bool ev##.altKey then Event.Ignore
    else (
      match Dom_html.Keyboard_code.of_event ev with
      | ArrowUp    | KeyK -> ignore_if is_input (move_focus_row Prev)
      | ArrowDown  | KeyJ -> ignore_if is_input (move_focus_row Next)
      | ArrowLeft  | KeyH -> ignore_if is_input (move_focus_col Prev)
      | ArrowRight | KeyL -> ignore_if is_input (move_focus_col Next)
      | PageUp -> ignore_if is_input (page_focus_row Prev)
      | PageDown -> ignore_if is_input (page_focus_row Next)
      | Escape -> ignore_if is_search_input (inject Action.Escape)
      | Enter -> ignore_if is_search_input (inject Action.Commit_edits)
      | KeyE -> ignore_if (is_input || not (Js.to_bool ev##.shiftKey)) (inject Edit_start)
      | KeyS -> ignore_if is_input (inject Action.Add_focused_sort_col)
      | _ -> Event.Ignore
    )
  )

let row_renderer
      (m : Model.t Incr.t)
      ~(inject : Action.t -> Vdom.Event.t)
      : Row.Model.t Ts_table.row_renderer
  =
  let table_m = m >>| Model.table in
  let sort_columns = table_m >>| Ts_table.Model.sort_columns in
  Incr.set_cutoff sort_columns (Incr.Cutoff.of_compare [%compare: int list]);
  let focused_column = table_m >>| Ts_table.Model.focus_col in
  let focused_row = table_m >>| Ts_table.Model.focus_row in
  let edit_state = m >>| Model.edit_state in
  (fun ~row_id ~row ->
     let mode =
       let%bind focused_row = focused_row in
       let focused = [%compare.equal:Row_id.t option] (Some row_id) focused_row in
       if not focused then (Incr.const Row.Mode.Unfocused)
       else (
         match%map edit_state with
         | Editing _ -> Row.Mode.Editing
         | Not_editing -> Row.Mode.Focused
       )
     in
     let focus_me =
       inject (Action.Table_action (Ts_table.Action.set_focus_row (Some row_id)))
     in
     let focus_nth_column col_num =
       inject (Action.Table_action (Ts_table.Action.set_focus_col (Some col_num)))
     in
     let%bind sort_columns = sort_columns in
     Row.view
       row
       ~mode
       ~sort_columns
       ~focused_column
       ~focus_me
       ~focus_nth_column
       ~remember_edit:(fun ~column value -> inject (Remember_edit {column;value}))
  )

let view
      (m : Model.t Incr.t)
      (d : Derived_model.t Incr.t)
      ~(inject : Action.t -> Vdom.Event.t)
  =
  let open Vdom in
  let scroll_attr = Vdom.Attr.on "scroll" (fun _ -> Vdom.Event.Viewport_changed) in
  let key_handler = key_handler ~inject in
  let input =
    Node.div [ Attr.id "search-container"]
      [
        Node.input
          [ Attr.id search_input_id
          ; Attr.create "placeholder" "Search"
          ; Attr.create "type" "text"
          ; Attr.on_input (fun _ev text -> inject (Set_pattern text)) ]
          []
      ]
  in
  let table_m = m >>| Model.table in
  let table_d = d >>| Derived_model.table in
  let render_row = row_renderer m ~inject in
  let%map table =
    Ts_table.view table_m table_d
      ~render_row
      ~inject:(fun a -> inject (Action.Table_action a))
      ~attrs:[ Attr.class_ "table table-bordered"]
  in
  Node.body [ scroll_attr; key_handler]
    [ Node.div [ Attr.id "table-container" ]
        [ input
        ; Node.div [] [ table ]
        ]
    ; Node.div [ Attr.class_ "big-gap" ] []
    ]
;;

let should_set_edit_focus ~old (m:Model.t) =
  let focus = Ts_table.Model.focus_row m.table in
  let is_editing (m:Model.t) =
    match m.edit_state, focus with
    | Not_editing, _ | _, None -> false
    | Editing _, Some _ -> true
  in
  let edit_state m =
    (is_editing m, focus)
  in
  is_editing m
  && not ([%compare.equal: bool * Row_id.t option]
            (edit_state old)
            (edit_state m))
;;

let set_edit_focus (m:Model.t) =
  let focus = Ts_table.Model.focus_row m.table in
  match m.edit_state, focus with
  | Not_editing, _ | _, None -> ()
  | Editing _, Some _ ->
    match Dom_html.getElementById_coerce "focus-on-edit" Dom_html.CoerceTo.input with
    | None -> ()
    | Some i -> i##select
;;

let maybe_set_edit_focus ~old m =
  if should_set_edit_focus ~old m then (set_edit_focus m)

let on_display
      ~(old : Model.t)
      (m : Model.t)
      (d : Derived_model.t)
      (_state : State.t)
  =
  (* If the focus has moved, and is now outside the visible range, scroll until the
     focused point is back in view.  *)
  maybe_set_edit_focus ~old m;
  let editing (model:Model.t) =
    match model.edit_state with
    | Not_editing -> false
    | Editing _   -> true
  in
  (* When the user presses the shift+e when the focus is out of view, scroll to it. *)
  if (not (editing old) && editing m) then (
    ignore (Ts_table.scroll_focus_into_scroll_region m.table d.table)
  ) else (
    (* Because we don't re-measure the viewport, if the app is slow and a focus change
       gets batched with an edit, without the [else] it will scroll relatively twice. *)
    Ts_table.on_display ~old:old.table m.table d.table
  )
;;

let on_startup ~schedule (m:Model.t) (_:Derived_model.t) =
  let state = { State.schedule } in
  let open Async_kernel in
  let ids = Map.keys m.rows |> Array.of_list in
  every (Time_ns.Span.of_ms 50.) (fun () -> State.schedule state (Big_kick ids));
  return state

let height_guess = 43.

let init () : Model.t =
  let rows =
    Row.random_rows 10_000
    |> List.map ~f:(fun row -> (Row_id.create (), row))
    |> Row_id.Map.of_alist_exn
  in
  let table =
    Ts_table.Model.create
      ~scroll_margin:(Table.Margin.uniform 5.)
      ~scroll_region:Window
      ~float_header:Edge
      ~float_first_col:(Px_from_edge (-1))
      ~height_guess
      ()
  in
  { rows
  ; pattern = ""
  ; edit_state = Not_editing
  ; table
  }
