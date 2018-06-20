open! Core_kernel
open! Import
open Incr_dom_widgets

module Model = struct
  type edit = { column: string; value: string }
  [@@deriving compare, sexp]

  type edit_state =
    | Not_editing
    | Editing of edit list
  [@@deriving compare, sexp]

  type t =
    { rows       : Row.Model.t Row_id.Map.t
    ; pattern    : string
    ; edit_state : edit_state
    ; table      : Ts_table.Model.t
    ; help_text  : Help_text.t option
    }
  [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0

  let set_pattern t pattern = { t with pattern = String.lowercase pattern }
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
    | Show_help_menu of Help_text.t
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
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

let show_help_menu (m:Model.t) help_text =
  { m with help_text = Some help_text }

let escape (m:Model.t) (d:Derived_model.t) =
  if Option.is_some m.help_text
  then { m with help_text = None }
  else (
    match m.edit_state with
    | Not_editing ->
      let action = Ts_table.Action.set_focus_row None in
      { m with table = (Ts_table.apply_action m.table d.table action) }
    | Editing _ -> { m with edit_state = Not_editing }
  )

let big_kick (m:Model.t) ids =
  let num = Array.length ids / 120 in
  Sequence.fold (Sequence.range 1 num) ~init:m ~f:(fun m i ->
    let id = ids.(Random.int (Array.length ids)) in
    let m = apply_row_action m id Row.Action.kick_price in
    if i mod 3 <> 0 then m
    else (apply_row_action m id Row.Action.kick_position)
  )

let apply_action
      (action:Action.t) (m:Model.t) _ ~schedule_action:_
      ~(recompute_derived : Model.t -> Derived_model.t)
  =
  let d = lazy (recompute_derived m) in
  match action with
  | Escape                   -> escape m (force d)
  | Set_pattern s            -> Model.set_pattern m s
  | Table_action a           ->
    {m with table = (Ts_table.apply_action m.table (force d).table a)}
  | Row_action (id,x)        -> apply_row_action m id x
  | Big_kick ids             -> big_kick m ids
  | Edit_start               -> { m with edit_state = Editing [] }
  | Remember_edit edit       -> remember_edit m edit
  | Commit_edits             -> commit_edits m
  | Add_focused_sort_col     -> add_focused_sort_col m
  | Show_help_menu help_text -> show_help_menu m help_text

let search_input_id = "search-input"

let update_visibility (m:Model.t) (d:Derived_model.t) ~recompute_derived:_ =
  let table = Ts_table.update_visibility m.table d.table in
  {m with table}

let key_handler ~inject =
  let open Vdom in
  let open Keyboard_event_handler.Condition in
  let command ?cond ~keys ~description f =
    let handler =
      let open Keyboard_event_handler.Handler in
      match cond with
      | None      -> with_prevent_default f
      | Some cond -> only_handle_if cond f ~prevent_default:()
    in
    { Keyboard_event_handler.Command. keys; description; group = None; handler }
  in
  let key = Keystroke.create' in
  let is_not_text_input = not_ has_text_input_target in
  let is_not_search_input = not_ (has_target_id ~id:search_input_id) in
  let inject' table_action = inject (Action.Table_action table_action) in
  let handler =
    Keyboard_event_handler.of_command_list_exn
      [ command
          ~keys:[ key ArrowUp; key KeyK ]
          ~description:"Move focus one row up"
          ~cond:is_not_text_input
          (fun _ev -> inject' (Ts_table.Action.move_focus_row Prev))
      ; command
          ~keys:[ key ArrowDown; key KeyJ ]
          ~description:"Move focus one row down"
          ~cond:is_not_text_input
          (fun _ev -> inject' (Ts_table.Action.move_focus_row Next))
      ; command
          ~keys:[ key ArrowLeft; key KeyH ]
          ~description:"Move focus one column left"
          ~cond:is_not_text_input
          (fun _ev -> inject' (Ts_table.Action.move_focus_col Prev))
      ; command
          ~keys:[ key ArrowRight; key KeyL ]
          ~description:"Move focus one column right"
          ~cond:is_not_text_input
          (fun _ev -> inject' (Ts_table.Action.move_focus_col Next))
      ; command
          ~keys:[ key PageUp ]
          ~description:"Move focus one page up"
          ~cond:is_not_text_input
          (fun _ev -> inject' (Ts_table.Action.page_focus_row Prev))
      ; command
          ~keys:[ key PageDown ]
          ~description:"Move focus one page down"
          ~cond:is_not_text_input
          (fun _ev -> inject' (Ts_table.Action.page_focus_row Next))
      ; command
          ~keys:[ key Escape ]
          ~description:"Close help menu, cancel editing, or remove focus"
          ~cond:is_not_search_input
          (fun _ev -> inject Action.Escape)
      ; command
          ~keys:[ key Enter ]
          ~description:"Commit edits"
          ~cond:is_not_search_input
          (fun _ev -> inject Action.Commit_edits)
      ; command
          ~keys:[ key ~shift:() KeyE ]
          ~description:"Start editing focused row"
          ~cond:is_not_text_input
          (fun _ev -> inject Action.Edit_start)
      ; command
          ~keys:[ key KeyS ]
          ~description:"Sort on focused column (in addition to existing sort columns)"
          ~cond:is_not_text_input
          (fun _ev -> inject Action.Add_focused_sort_col)
      ]
  in
  let help_menu_command =
    command
      ~keys:[ key F1; key ~ctrl:() ~shift:() Slash ]
      ~description:"See the help menu"
      (fun _ev ->
         let help_text = Keyboard_event_handler.get_help_text handler in
         inject (Action.Show_help_menu help_text)
      )
  in
  let handler = Keyboard_event_handler.add_command_exn handler help_menu_command in
  let keydown_handler =
    Attr.on_keydown (fun ev ->
      Option.value (Keyboard_event_handler.handle_event handler ev)
        ~default:Event.Ignore
    )
  in
  keydown_handler, help_menu_command

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
  let key_handler, help_menu_command = key_handler ~inject in
  let help_text_view_spec =
    Help_text.View_spec.with_classes
      ~key_class:"help-text-key"
      ~plain_text_class:"help-text-plain-text"
  in
  let help_menu_hint =
    let help_text = Keyboard_event_handler.Command.get_help_text help_menu_command in
    Node.div [ Attr.style [ "text-align", "center"; "padding", "5px" ] ]
      [ Help_text.Command.(view help_text help_text_view_spec Format.default) ]
  in
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
      ~attrs:[ Attr.class_ "table table-bordered" ]
  and help_menu =
    match%map m >>| Model.help_text with
    | None -> []
    | Some help_text ->
      [ Node.div
          [ Attr.id "overlay"
          ; Attr.on_double_click (fun _ev -> inject Action.Escape)
          ]
          [ Node.div [ Attr.id "help-menu" ]
              [ Node.h4 [] [ Node.text "Help Menu" ]
              ; Help_text.view help_text help_text_view_spec
              ]
          ]
      ]
  in
  Node.body [ scroll_attr; key_handler]
    (help_menu @
     [ Node.div [ Attr.id "table-container" ]
         [ input
         ; help_menu_hint
         ; Node.div [] [ table ]
         ]
     ; Node.div [ Attr.class_ "big-gap" ] []
     ]
    )
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
      ~old
      (m : Model.t)
      (d : Derived_model.t)
      (_state : State.t)
      ~schedule_action:_
  =
  (* If the focus has moved, and is now outside the visible range, scroll until the
     focused point is back in view.  *)
  let old = fst old in
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

let on_startup ~(schedule_action:Action.t -> unit) (m:Model.t) (_:Derived_model.t) =
  let open Async_kernel in
  let ids = Map.keys m.rows |> Array.of_list in
  every (Time_ns.Span.of_ms 50.) (fun () -> schedule_action (Big_kick ids));
  Deferred.unit

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
  ; help_text = None
  }
