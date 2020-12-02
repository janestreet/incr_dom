open! Core_kernel
open Poly
open! Import
open Vdom_keyboard

module Model = struct
  type edit =
    { column : string
    ; value : string
    }
  [@@deriving compare, sexp]

  type edit_state =
    | Not_editing
    | Editing of edit list
  [@@deriving compare, sexp]

  type t =
    { rows : Row.Model.t Row_id.Map.t
    ; pattern : string
    ; edit_state : edit_state
    ; table : Ts_table.Model.t
    ; help_text : Help_text.t option
    }
  [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0
  let set_pattern t pattern = { t with pattern = String.lowercase pattern }
end

module Action = struct
  type t =
    | Escape
    | Set_pattern of string
    | Table_action of Ts_table.Action.t
    | Row_action of Row_id.t * Row.Action.t
    | Big_kick of (Row_id.t array[@sexp.opaque])
    | Edit_start
    | Remember_edit of Model.edit
    | Commit_edits
    | Add_focused_sort_col
    | Show_help_menu of Help_text.t
    | Add_row
    | Remove_row
  [@@deriving sexp, variants]
end

module State = struct
  type t = unit
end

let apply_row_action (m : Model.t) row_id (action : Row.Action.t) =
  let rows =
    Map.change m.rows row_id ~f:(function
      | None -> None
      | Some row -> Some (Row.apply_action action row))
  in
  { m with rows }
;;

let remember_edit (m : Model.t) (new_edit : Model.edit) =
  match m.edit_state with
  | Not_editing -> m
  | Editing edits ->
    let edits = List.filter edits ~f:(fun edit -> edit.column <> new_edit.column) in
    { m with edit_state = Editing (new_edit :: edits) }
;;

let commit_edits (m : Model.t) =
  let m =
    match m.edit_state with
    | Not_editing -> m
    | Editing edits ->
      (match Ts_table.Model.focus_row m.table with
       | None -> m
       | Some row_id ->
         let rows =
           Map.change m.rows row_id ~f:(function
             | None -> None
             | Some row ->
               Some
                 (List.fold edits ~init:row ~f:(fun r e ->
                    Row.Model.apply_edit r ~column:e.column e.value)))
         in
         { m with rows })
  in
  { m with edit_state = Not_editing }
;;

let add_focused_sort_col (m : Model.t) =
  match Ts_table.Model.focus_col m.table with
  | None -> m
  | Some col_id ->
    let table =
      Ts_table.Model.cycle_sorting
        m.table
        col_id
        ~next_dir:Ts_table.Sort_dir.next
        ~keep_existing_cols:()
    in
    { m with table }
;;

let show_help_menu (m : Model.t) help_text = { m with help_text = Some help_text }

let add_row (m : Model.t) =
  { m with rows = Map.set m.rows ~key:(Row_id.create ()) ~data:(Row.random_row ()) }
;;

let remove_row (m : Model.t) =
  match List.random_element (Map.keys m.rows) with
  | None -> m
  | Some row_id ->
    let rows = Map.remove m.rows row_id in
    let table =
      if Option.equal Row_id.equal (Some row_id) (Ts_table.Model.focus_row m.table)
      then Ts_table.set_focus_col (Ts_table.set_focus_row m.table None) None
      else m.table
    in
    { m with rows; table }
;;

let escape table_apply_action (m : Model.t) =
  if Option.is_some m.help_text
  then { m with help_text = None }
  else (
    match m.edit_state with
    | Not_editing ->
      let action = Ts_table.Action.set_focus_row None in
      { m with table = table_apply_action action }
    | Editing _ -> { m with edit_state = Not_editing })
;;

let big_kick (m : Model.t) ids =
  let num = Array.length ids / 120 in
  Sequence.fold (Sequence.range 1 num) ~init:m ~f:(fun m i ->
    let id = ids.(Random.int (Array.length ids)) in
    let m = apply_row_action m id Row.Action.kick_price in
    if i mod 3 <> 0 then m else apply_row_action m id Row.Action.kick_position)
;;

let apply_action table (m : Model.t Incr.t) =
  let%map m = m
  and table_apply_action = table >>| Component.apply_action in
  fun (action : Action.t) state ~schedule_action ->
    let schedule_table_action action = schedule_action (Action.Table_action action) in
    let apply_table_action action =
      table_apply_action action state ~schedule_action:schedule_table_action
    in
    match action with
    | Escape -> escape apply_table_action m
    | Set_pattern s -> Model.set_pattern m s
    | Table_action a -> { m with table = apply_table_action a }
    | Row_action (id, x) -> apply_row_action m id x
    | Big_kick ids -> big_kick m ids
    | Edit_start -> { m with edit_state = Editing [] }
    | Remember_edit edit -> remember_edit m edit
    | Commit_edits -> commit_edits m
    | Add_focused_sort_col -> add_focused_sort_col m
    | Show_help_menu help_text -> show_help_menu m help_text
    | Add_row -> add_row m
    | Remove_row -> remove_row m
;;

let search_input_id = "search-input"

let update_visibility table (m : Model.t Incr.t) =
  let%map m = m
  and table = table in
  fun ~schedule_action ->
    let table =
      Component.update_visibility
        table
        ~schedule_action:(Fn.compose schedule_action Action.table_action)
    in
    { m with table }
;;

let key_handler ~inject =
  let open Vdom in
  let open Keyboard_event_handler.Condition in
  let command ?cond ~keys ~description f =
    let handler =
      let open Keyboard_event_handler.Handler in
      match cond with
      | None -> with_prevent_default f
      | Some cond -> only_handle_if cond f ~prevent_default:()
    in
    { Keyboard_event_handler.Command.keys; description; group = None; handler }
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
      ; command
          ~keys:[ key ~alt:() KeyA ]
          ~description:"Add a randomly-generated row"
          ~cond:is_not_text_input
          (fun _ev -> inject Action.Add_row)
      ; command
          ~keys:[ key ~alt:() KeyR ]
          ~description:"Remove a randomly chosen row"
          ~cond:is_not_text_input
          (fun _ev -> inject Action.Remove_row)
      ]
  in
  let help_menu_command =
    command
      ~keys:[ key F1; key ~ctrl:() ~shift:() Slash ]
      ~description:"See the help menu"
      (fun _ev ->
         let help_text = Keyboard_event_handler.get_help_text handler in
         inject (Action.Show_help_menu help_text))
  in
  let handler = Keyboard_event_handler.add_command_exn handler help_menu_command in
  let keydown_handler =
    Attr.on_keydown (fun ev -> Keyboard_event_handler.handle_or_ignore_event handler ev)
  in
  keydown_handler, help_menu_command
;;

let row_renderer (m : Model.t Incr.t) ~(inject : Action.t -> Vdom.Event.t)
  : Row.Model.t Ts_table.row_renderer
  =
  let table_m = m >>| Model.table in
  let sort_columns = table_m >>| Ts_table.Model.sort_columns in
  Incr.set_cutoff sort_columns (Incr.Cutoff.of_compare [%compare: int list]);
  let focused_column = table_m >>| Ts_table.Model.focus_col in
  let focused_row = table_m >>| Ts_table.Model.focus_row in
  let edit_state = m >>| Model.edit_state in
  fun ~row_id ~row ->
    let mode =
      let%bind focused_row = focused_row in
      let focused = [%compare.equal: Row_id.t option] (Some row_id) focused_row in
      if not focused
      then Incr.const Row.Mode.Unfocused
      else (
        match%map edit_state with
        | Editing _ -> Row.Mode.Editing
        | Not_editing -> Row.Mode.Focused)
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
      ~remember_edit:(fun ~column value -> inject (Remember_edit { column; value }))
;;

let view table (m : Model.t Incr.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let scroll_attr = Vdom.Attr.on_scroll (fun _ -> Vdom.Event.Viewport_changed) in
  let key_handler, help_menu_command = key_handler ~inject in
  let help_text_view_spec =
    Help_text.View_spec.with_classes
      ~key_class:"help-text-key"
      ~plain_text_class:"help-text-plain-text"
  in
  let help_menu_hint =
    let help_text = Keyboard_event_handler.Command.get_help_text help_menu_command in
    Node.div
      [ Attr.style Css_gen.(combine (text_align `Center) (uniform_padding (`Px 5))) ]
      [ Help_text.Command.(view help_text help_text_view_spec Format.default) ]
  in
  let input =
    Node.div
      [ Attr.id "search-container" ]
      [ Node.input
          [ Attr.id search_input_id
          ; Attr.create "placeholder" "Search"
          ; Attr.create "type" "text"
          ; Attr.on_input (fun _ev text -> inject (Set_pattern text))
          ]
          []
      ]
  in
  let%map table = table >>| Component.view
  and maybe_help_menu =
    match%map m >>| Model.help_text with
    | None -> Node.none
    | Some help_text ->
      Node.div
        ~key:"help"
        [ Attr.id "overlay"; Attr.on_double_click (fun _ev -> inject Action.Escape) ]
        [ Node.div
            [ Attr.id "help-menu" ]
            [ Node.h4 [] [ Node.text "Help Menu" ]
            ; Help_text.view help_text help_text_view_spec
            ]
        ]
  in
  Node.div
    [ Attr.id "app"; key_handler ]
    [ maybe_help_menu
    ; Node.div ~key:"top" [ Attr.id "top-container" ] [ input; help_menu_hint ]
    ; Node.div ~key:"table" [ Attr.id "table-container"; scroll_attr ] [ table ]
    ]
;;

let should_set_edit_focus ~old_model (m : Model.t) =
  let focus = Ts_table.Model.focus_row m.table in
  let is_editing (m : Model.t) =
    match m.edit_state, focus with
    | Not_editing, _ | _, None -> false
    | Editing _, Some _ -> true
  in
  let edit_state m = is_editing m, focus in
  is_editing m
  && not ([%compare.equal: bool * Row_id.t option] (edit_state old_model) (edit_state m))
;;

let set_edit_focus (m : Model.t) =
  let focus = Ts_table.Model.focus_row m.table in
  match m.edit_state, focus with
  | Not_editing, _ | _, None -> ()
  | Editing _, Some _ ->
    (match Dom_html.getElementById_coerce "focus-on-edit" Dom_html.CoerceTo.input with
     | None -> ()
     | Some i -> i##select)
;;

let maybe_set_edit_focus ~old_model m =
  if should_set_edit_focus ~old_model m then set_edit_focus m
;;

let on_display table ~old_model (m : Model.t Incr.t) =
  let%map table_on_display = table >>| Component.on_display
  and old_model = old_model
  and table_extra = table >>| Component.extra
  and m = m in
  fun state ~schedule_action ->
    (* If the focus has moved, and is now outside the visible range, scroll until the
       focused point is back in view.  *)
    maybe_set_edit_focus ~old_model m;
    let editing (model : Model.t) =
      match model.edit_state with
      | Not_editing -> false
      | Editing _ -> true
    in
    (* When the user presses the shift+e when the focus is out of view, scroll to it. *)
    if (not (editing old_model)) && editing m
    then ignore (Ts_table.Extra.scroll_focus_into_scroll_region m.table table_extra)
    else (
      (* Because we don't re-measure the viewport, if the app is slow and a focus change
         gets batched with an edit, without the [else] it will scroll relatively twice. *)
      let schedule_table_action action = schedule_action (Action.Table_action action) in
      table_on_display state ~schedule_action:schedule_table_action)
;;

let on_startup ~schedule_action (m : Model.t) =
  let open Async_kernel in
  let ids = Map.keys m.rows |> Array.of_list in
  every (Time_ns.Span.of_ms 50.) (fun () -> schedule_action (Action.Big_kick ids));
  Deferred.unit
;;

let height_guess = 43.

let create_table_component (model : Model.t Incr.t) ~old_model ~inject =
  let columns = List.map ~f:Column.to_table_widget_column Row.Model.columns in
  let columns = Incr.const (List.mapi columns ~f:(fun i col -> i, col)) in
  let rows =
    let%bind pattern = model >>| Model.pattern in
    Incr.Map.filter_mapi (model >>| Model.rows) ~f:(fun ~key:_ ~data ->
      Option.some_if (Row.Model.matches_pattern data pattern) data)
  in
  let render_row = row_renderer model ~inject in
  let table_model = model >>| Model.table in
  let old_table_model = old_model >>| Model.table >>| Option.some in
  Ts_table.create
    table_model
    ~old_model:old_table_model
    ~rows
    ~columns
    ~render_row
    ~inject:(fun a -> inject (Action.Table_action a))
    ~attrs:[ Vdom.Attr.classes [ "table"; "table-bordered" ] ]
;;

let create model ~old_model ~inject =
  let table = create_table_component model ~old_model ~inject in
  let%map on_display = on_display table ~old_model model
  and apply_action = apply_action table model
  and update_visibility = update_visibility table model
  and view = view table model ~inject
  and model = model in
  Component.create ~apply_action ~update_visibility ~on_display model view
;;

let init () : Model.t =
  let rows =
    Row.random_rows 10_000
    |> List.map ~f:(fun row -> Row_id.create (), row)
    |> Row_id.Map.of_alist_exn
  in
  let table =
    Ts_table.Model.create
      ~scroll_margin:(Table.Margin.uniform 5.)
      ~scroll_region:(Element "table-container")
      ~float_header:Edge
      ~float_first_col:(Px_from_edge (-1))
      ~height_guess
      ()
  in
  { rows; pattern = ""; edit_state = Not_editing; table; help_text = None }
;;
