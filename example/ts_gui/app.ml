open! Core_kernel.Std
open! Import

let debug = false

module Row_id = struct
  include (Unique_id.Int ())
  let to_dom x = "row-" ^ to_string x
end

module Sort_dir = struct
  type t = Ascending | Descending
  [@@deriving sexp, compare]

  let sign = function Ascending -> 1 | Descending -> -1
  let flip = function Ascending -> Descending | Descending -> Ascending
end

module Key : sig
  type t = { sort_key: Sort_key.t
           ; row_id: Row_id.t
           ; sort_dir: Sort_dir.t
           }
  [@@deriving sexp]
  include Comparable with type t := t

  module Ascending : Comparator.S with type t = t
  module Descending : Comparator.S with type t = t
end = struct
  module T = struct
    type t = { sort_key: Sort_key.t
             ; row_id: Row_id.t
             ; sort_dir: Sort_dir.t
             }
    [@@deriving sexp, compare]

    (** The comparison function here is written so that any two keys with the same
        sort_dir sort according to that sort_dir; but keys with different sort_dirs just
        have a stable relation between them. This allows us to have one Key type that is
        used by all the different sorting situations, without needing to change
        comparators. *)
    let compare t1 t2 =
      match Sort_dir.compare t1.sort_dir t2.sort_dir with
      | 0 -> Sort_dir.sign t1.sort_dir * compare t1 t2
      | c -> c
  end
  include T
  include Comparable.Make(T)

  module Ascending = struct
    type nonrec t = t
    include Comparator.Make(T)
  end

  module Descending = struct
    type nonrec t = t
    include Comparator.Make(struct
        include T
        let compare x y = compare y x
      end)
  end
end


let spacing = 2


module Heights = struct
  type height_and_count = { height: int; count: int }

  include Splay_tree.Make_with_reduction (Key) (Int) (struct
      type key = Key.t
      type data = int (* height *)
      type accum = height_and_count
      let identity = { height = 0; count = 0 }
      let singleton ~key:_ ~data = { height = data; count = 1 }
      let combine left right =
        { height = left.height + right.height
        ; count = left.count + right.count
        }
    end)

  (** Returns the row (if any) that is at the specified height *)
  let find_by_height (heights:t) ~spacing height =
    search heights ~f:(fun ~left ~right:_ ->
      (* The half-space is used to determine what happens if the height being searched for
         falls within the space between two rows. Then if it is in the top half of the
         space the row above it is returned, and if it is in the bottom half of the space
         the row below it is returned.  *)
      let half_space = (spacing + 1) / 2 in
      let left = left.height + left.count * spacing + half_space in
      if height < left then `Left else `Right
    )
    |> Option.map ~f:fst

  let height t ~spacing =
    let {height;count} = accum t in
    height + count * spacing
end

module With_height = struct
  type 'a t = { value: 'a ; height: int }
  [@@deriving fields, compare]

  let create value ~height = { value; height }
end

module Visibility_info = struct
  type t =
    { tbody_rect: int Js_misc.Rect.t
    ; main_rect: int Js_misc.Rect.t
    }
  [@@deriving compare, sexp]
end

module Model = struct
  type edit = { column: string; value: string }
  [@@deriving compare, sexp]

  type edit_state =
    | Not_editing
    | Editing of edit list
  [@@deriving compare, sexp]

  type t =
    { rows: Row.Model.t With_height.t Row_id.Map.t
    ; focus: Row_id.t option
    ; pattern: string
    ; sort_column : int option
    ; sort_dir : Sort_dir.t
    ; edit_state : edit_state
    ; visibility_info : Visibility_info.t option
    }
  [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0

  let escape t =
    match t.edit_state with
    | Not_editing -> { t with focus = None }
    | Editing _ -> { t with edit_state = Not_editing }

  let set_pattern t pattern = { t with pattern = String.lowercase pattern }

  let set_sort_column t i =
    let sort_column = Some i in
    let sort_dir =
      match t.sort_column with
      | None -> Sort_dir.Ascending
      | Some old ->
        if old = i then (Sort_dir.flip t.sort_dir)
        else Sort_dir.Ascending
    in
    { t with sort_column; sort_dir }

  let sort_key ~column row row_id : Sort_key.t =
    let column =
      let open Option.Let_syntax in
      let%bind i = column in
      List.nth Row.Model.columns i
    in
    match column with
    | None -> String (Row_id.to_string row_id)
    | Some col -> Column.sort_by col row
  ;;

  (** Compute the key directly from the required values, without the model.  *)
  let key_direct ~column sort_dir row row_id =
    let sort_key = sort_key ~column row row_id in
    { Key. sort_key; row_id; sort_dir }
  ;;

  (** Compute the key given the model *)
  let key t row_id =
    Option.map (Map.find t.rows row_id) ~f:(fun data ->
      key_direct ~column:t.sort_column t.sort_dir data.value row_id)

end

module Model_summary = struct
  type t = Model.t
  let create (m:Model.t) _ = m
end

module Interval = struct
  type 'a t =
    | Empty
    | Range of 'a * 'a
  [@@deriving compare, sexp]
end

module Derived_model = struct
  type t = { filtered_rows : Row.Model.t With_height.t Key.Map.t
           ; heights : Heights.t
           ; render_range:  Key.t Interval.t
           (** When [None], nothing is visible so nothing will be rendered *)
           ; visible_range: Key.t Interval.t
           (** The top and bottom of the viewport, measured from the top of the table *)
           ; scroll_pos : int * int
           }
  [@@deriving fields]

  let visible_range_and_scroll_pos
        (m:Model.t Incr.t)
        ~(heights:Heights.t Incr.t)
        ~(filtered_rows:Row.Model.t With_height.t Key.Map.t Incr.t)
    =
    let%map visibility_info = m >>| Model.visibility_info
    and heights = heights
    and filtered_rows = filtered_rows
    in
    match visibility_info with
    | None -> (Interval.Empty,(0,0))
    | Some {tbody_rect; main_rect} ->
      let module Rect = Js_misc.Rect in
      (* The top of the viewport, as measured from the top of the table body. Note that
         the top of tboyd_rect is measured against the viewport, and so is a negative
         number when you're scrolled into the middle of the table.  *)
      let scroll_top = -(Rect.top tbody_rect) in
      (* The height of the table, which excludes the height of the header *)
      let scroll_bot = scroll_top + Rect.height main_rect in
      let scroll_pos = (scroll_top, scroll_bot) in
      let key_top =
        match Heights.find_by_height heights ~spacing scroll_top with
        | Some x -> Some x
        | None -> Map.min_elt filtered_rows |> Option.map ~f:fst
      in
      let key_bot =
        match Heights.find_by_height heights ~spacing scroll_bot with
        | Some x -> Some x
        | None -> Map.max_elt filtered_rows |> Option.map ~f:fst
      in
      let visible_range : _ Interval.t =
        if scroll_top >= Heights.height ~spacing heights then Empty
        else (
          match key_top, key_bot with
          | None,_ | _, None -> Empty
          | Some top, Some bot -> Range (top,bot)
        )
      in
      if debug then (
        logf "%s" (Sexp.to_string_hum [%message
                     "visible range"
                       (visible_range : Key.t Interval.t)
                       (main_rect : int Rect.t)
                       (tbody_rect : int Rect.t)
                       (scroll_top : int)
                       (scroll_bot : int)
                   ])
      );
      (visible_range, scroll_pos)
  ;;

  (** How many extra rows will be rendered outside of visible range *)
  let render_width = 20

  let create (m:Model.t Incr.t) =
    let filtered_unsorted_rows =
      let%bind pattern = m >>| Model.pattern in
      Incr.Map.filter_mapi (m >>| Model.rows) ~f:(fun ~key:_ ~data ->
        Option.some_if (Row.Model.matches_pattern data.value pattern) data)
    in
    let filtered_rows =
      let%bind column = m >>| Model.sort_column
      and sort_dir = m >>| Model.sort_dir
      in
      Incr.Map.unordered_fold filtered_unsorted_rows
        ~init:Key.Map.empty
        ~f:(fun ~key:row_id ~data acc ->
          let key = Model.key_direct ~column sort_dir data.value row_id in
          Map.add acc ~key ~data)
        ~f_inverse:(fun ~key:row_id ~data acc ->
          let key = Model.key_direct ~column sort_dir data.value row_id in
          Map.remove acc key)
    in
    let heights =
      Incr.Map.unordered_fold filtered_rows
        ~init:Heights.empty
        ~f:(fun ~key ~data acc -> Heights.set acc ~key ~data:data.height)
        ~f_inverse:(fun ~key ~data:_ acc -> Heights.remove acc key)
    in
    let just_keys =
      Incr.Map.mapi ~data_equal:(fun _ _ -> true)
        filtered_rows ~f:(fun ~key:_ ~data:_ -> ())
    in
    let visible_range_and_scroll_pos =
      visible_range_and_scroll_pos m ~heights ~filtered_rows
    in
    let render_range =
      let%map (visible_range,_) = visible_range_and_scroll_pos
      and just_keys = just_keys
      in
      let rec move start n get_next =
        if n <= 0 then start
        else (
          match get_next start with
          | None -> start
          | Some next -> move next (n - 1) get_next
        )
      in
      let move start direction =
        move start render_width (fun x ->
          Map.closest_key just_keys direction x
          |> Option.map ~f:fst)
      in
      match (visible_range : _ Interval.t) with
      | Empty -> Interval.Empty
      | Range (top,bot) ->
        Interval.Range ( move top `Less_than
                       , move bot `Greater_than )
    in
    let%map heights = heights
    and filtered_rows = filtered_rows
    and render_range = render_range
    and (visible_range,scroll_pos) = visible_range_and_scroll_pos
    in
    { filtered_rows; heights; render_range; visible_range; scroll_pos }
end

module Action = struct
  type t =
    | Move_focus of Focus.dir
    | Escape
    | Set_focus of Row_id.t
    | Set_pattern of string
    | Set_sort_column of int
    | Row_action of Row_id.t * Row.Action.t
    | Edit_start
    | Remember_edit of Model.edit
    | Commit_edits
  [@@deriving sexp]

  let should_log = function
    | Row_action _ -> false
    | _ -> true
end

module State = struct
  type t = { schedule : Action.t -> unit }
  [@@deriving fields]
end

let move_focus (m:Model.t) (d:Derived_model.t) dir =
  let open Option.Let_syntax in
  let focus =
    let%bind row_id = m.focus in
    Model.key m row_id
  in
  let focus =
    match d.visible_range with
    | Empty -> None
    | Range (top,bot) ->
      let%map {row_id; _} =
        Focus.move_focus d.filtered_rows
          ~visible_range:(Some (top,bot))
          ~focus
          dir
      in
      row_id
  in
  { m with focus; edit_state = Not_editing }
;;

let set_focus (m:Model.t) (d:Derived_model.t) row_id =
  if [%compare.equal:Row_id.t option] m.focus (Some row_id)
  then m
  else (
    match Model.key m row_id with
    | None -> m
    | Some key ->
      if not (Map.mem d.filtered_rows key) then m
      else { m with focus = Some row_id; edit_state = Not_editing }
  )
;;

let apply_row_action (m:Model.t) row_id (action:Row.Action.t) =
  let rows = Map.change m.rows row_id ~f:(function
    | None -> None
    | Some row ->
      Some { row with value = Row.apply_action action row.value  })
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
      match m.focus with
      | None -> m
      | Some row_id ->
        let rows = Map.change m.rows row_id ~f:(function
          | None -> None
          | Some row ->
            Some { row with
                   value = List.fold edits ~init:row.value ~f:(fun r e ->
                     Row.Model.apply_edit r ~column:e.column e.value)})
        in
        { m with rows }
  in
  { m with edit_state = Not_editing }


let apply_action (action:Action.t) m _ ~stabilize_and_get_derived =
  let d = lazy (stabilize_and_get_derived ()) in
  match action with
  | Move_focus dir     -> move_focus m (force d) dir
  | Escape             -> Model.escape m
  | Set_focus s        -> set_focus m (force d) s
  | Set_pattern s      -> Model.set_pattern m s
  | Set_sort_column n  -> Model.set_sort_column m n
  | Row_action (id,x)  -> apply_row_action m id x
  | Edit_start         -> { m with edit_state = Editing [] }
  | Remember_edit edit -> remember_edit m edit
  | Commit_edits       -> commit_edits m

(** returns the element associated with the row id in question  *)
let find_row_element id =
  Option.try_with (fun () -> Dom_html.getElementById (Row_id.to_dom id))

(** Computes and updates the heights of all rows that are currently rendered *)
let update_heights (m : Model.t) (d : Derived_model.t) =
  match d.render_range with
  | Empty -> m
  | Range (min,max) ->
    let rows =
      Map.fold_range_inclusive d.filtered_rows ~min ~max
        ~init:m.rows
        ~f:(fun ~key ~data rows ->
          match find_row_element key.row_id with
          | None -> rows
          | Some el ->
            let height = Js_misc.Rect.height (Js_misc.viewport_rect_of_element el) in
            if height = data.height then rows
            else (Map.add rows ~key:key.row_id ~data:{ data with height }))
    in
    { m with rows }

let table_id = "main-table"
let tbody_id = "main-table-body"
let main_header_id = "main-header"
let search_input_id = "search-input"

(** Updates visibility information in the model. *)
let update_visibility (m:Model.t) (d:Derived_model.t) ~recompute_derived:_ =
  let m = update_heights m d in
  let tbody = Dom_html.getElementById tbody_id in
  let visibility_info =
    Some { Visibility_info.
           tbody_rect = Js_misc.viewport_rect_of_element tbody
         ; main_rect = Js_misc.viewport_rect ()
         }
  in
  if [%compare.equal: Visibility_info.t option] visibility_info m.visibility_info
  then m
  else { m with visibility_info }
;;

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
    let focus dir =
      Event.Many [ inject (Action.Move_focus dir); Event.Prevent_default ]
    in

    if Js.to_bool ev##.ctrlKey || Js.to_bool ev##.altKey then Event.Ignore
    else (
      match Dom_html.Keyboard_code.of_event ev with
      | ArrowUp   | KeyK -> ignore_if is_input (focus Prev)
      | ArrowDown | KeyJ -> ignore_if is_input (focus Next)
      | Escape -> ignore_if is_search_input (inject Action.Escape)
      | Enter -> ignore_if is_search_input (inject Action.Commit_edits)
      | KeyE -> ignore_if (is_input || not (Js.to_bool ev##.shiftKey)) (inject Edit_start)
      | _ -> Event.Ignore
    )
  )

let px_of_int x =
  Int.to_string x ^ "px"

let spacer ~key height =
  let open Vdom in
  [Node.tr ~key [ Attr.id ("spacer-" ^ key)
                ; Attr.style [ "height", px_of_int height ]] []]

(** Returns an incremental pair of heights to be used for the spacers *)
let spacer_heights render_range heights  =
  let%map render_range = render_range and heights = heights in
  let height = Heights.height ~spacing in
  match (render_range : _ Interval.t)  with
  | Empty -> (0, height heights)
  | Range (min_key, max_key) ->
    let { Heights.Partition.lt; gt; _ } =
      Heights.partition heights ~min_key ~max_key
    in
    (height lt, height gt)
;;

let view
      (m:Model.t Incr.t)
      (d : Derived_model.t Incr.t)
      ~(inject : Action.t -> Vdom.Event.t)
  =
  let open Vdom in
  let key_handler = key_handler ~inject in
  let get_focus =
    unstage
    @@ Incr.Select.select_one' (module Row_id) (m >>| Model.focus)
  in
  let get_mode row : Row.Mode.t Incr.t =
    let%bind focused = get_focus row in
    if not focused then (Incr.const Row.Mode.Unfocused)
    else (
      let%map m = m in
      match m.edit_state with
      | Editing _ -> Row.Mode.Editing
      | Not_editing -> Row.Mode.Focused
    )
  in
  let%bind sort_column_name, header =
    let%map sort_column_and_dir =
      let%map col = m >>| Model.sort_column
      and dir = m >>| Model.sort_dir in
      Option.map col ~f:(fun column -> (column, dir))
    in
    let sort_column_name =
      let open Option.Let_syntax in
      let%bind (i,_) = sort_column_and_dir in
      let%map col = List.nth Row.Model.columns i in
      Column.name col
    in
    let header =
      Node.tr [Attr.id main_header_id] (List.mapi Row.Model.columns ~f:(fun i column ->
        let suffix =
          match sort_column_and_dir with
          | None -> ""
          | Some (sort_column, dir) ->
            if Int.(=) i sort_column
            then begin match dir with Ascending -> "▲" | _ -> "▼" end
            else ""
        in
        Node.th
          [Attr.on_click (fun _ -> inject (Set_sort_column i))]
          [Node.text ((Column.name column) ^ suffix)]))
    in
    (sort_column_name, header)
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
  let heights = d >>| Derived_model.heights in
  let render_range = d >>| Derived_model.render_range in
  let spacer_heights = spacer_heights render_range heights in
  let%map rendered_rows =
    let render_range =
      match%map render_range with
      | Empty -> None
      | Range (x,y) -> Some (x,y)
    in
    Incr.Map.mapi'
      (Incr.Map.subrange (d >>| Derived_model.filtered_rows) render_range)
      ~f:(fun ~key ~data ->
        Row.view
          (data >>| fun x -> x.value)
          ~row_id:(Row_id.to_string key.row_id)
          ~mode:(get_mode key.row_id)
          ~focus_me:(inject (Set_focus key.row_id))
          ~sort_column:sort_column_name
          ~remember_edit:(fun ~column value -> inject (Remember_edit {column;value}))
      )
  and (before_height,after_height) = spacer_heights
  in
  let table =
    Node.table [Attr.id table_id; Attr.class_ "table table-bordered table-hover"]
      [ Node.thead [Attr.class_ "thead-default"] [header]
      ; Node.tbody [Attr.id tbody_id]
          (spacer ~key:"before" before_height
           @ Map.data rendered_rows
           @ spacer ~key:"after" after_height)
      ]
  in
  Node.body [key_handler]
    [ Node.div [Attr.id "table-container"]
        [ input
        ; Node.div [] [ table ]
        ]
    ]
;;

let ignore_unit_option (_:unit option) = ()

let should_set_edit_focus ~old m =
  let is_editing (m:Model.t) =
    match m.edit_state, m.focus with
    | Not_editing, _ | _, None -> false
    | Editing _, Some _ -> true
  in
  let edit_state m =
    (is_editing m, m.focus)
  in
  is_editing m
  && not ([%compare.equal: bool * Row_id.t option]
            (edit_state old)
            (edit_state m))
;;

let set_edit_focus (m:Model.t) =
  match m.edit_state, m.focus with
  | Not_editing, _ | _, None -> ()
  | Editing _, Some _ ->
    match Option.try_with (fun () -> Dom_html.getElementById "focus-on-edit") with
    | None -> ()
    | Some el ->
      match Dom_html.tagged el with
      | Input i ->
        i##select
      | _ -> ()
;;

let maybe_set_edit_focus ~old m =
  if should_set_edit_focus ~old m then (set_edit_focus m)

let on_display
      ~(old : Model.t)
      (m : Model.t)
      (d : Derived_model.t)
      (_state : State.t)
  =
  let open Option.Let_syntax in
  (* If the focus has moved, and is now outside the visible range, scroll until the
     focused point is back in view.  *)
  maybe_set_edit_focus ~old m;
  let editing (model:Model.t) =
    match model.edit_state with
    | Not_editing -> false
    | Editing _   -> true
  in
  if old.focus <> m.focus || (not (editing old) && editing m) then (
    ignore_unit_option @@
    let%bind row_id = m.focus in
    let%bind row = Map.find m.rows row_id in
    let%bind key = Model.key m row_id in
    let before, at, _ = Heights.split d.heights key in
    let%map (_:int) = at in
    let row_top = Heights.height ~spacing before in
    let row_bot = row_top + row.height in
    let (scroll_top, scroll_bot) = d.scroll_pos in
    if row_top < scroll_top
    then (Dom_html.window##scrollBy 0 (row_top - scroll_top))
    else if row_bot > scroll_bot
    then (Dom_html.window##scrollBy 0 (row_bot - scroll_bot))
  )
;;

let on_startup ~schedule (m:Model.t) (_:Derived_model.t) =
  let state = { State.schedule } in
  let open Async_kernel.Std in
  let ids = Map.keys m.rows |> Array.of_list in
  every (Time_ns.Span.of_ms 10.) (fun () ->
    for _i = 1 to Array.length ids / 100 do
      let id = ids.(Random.int (Array.length ids)) in
      State.schedule state (Row_action (id, Row.Action.kick_price))
    done);
  every (Time_ns.Span.of_ms 10.) (fun () ->
    for _i = 1 to Array.length ids / 1000 do
      let id = ids.(Random.int (Array.length ids)) in
      State.schedule state (Row_action (id, Row.Action.kick_fill_time));
    done);
  return state

let default_height = 20

let init () : Model.t =
  let rows =
    let height = default_height in
    Row.random_rows 10_000
    |> List.map ~f:(With_height.create ~height)
    |> List.map ~f:(fun row -> (Row_id.create (), row))
    |> Row_id.Map.of_alist_exn
  in
  { rows
  ; focus = None
  ; pattern = ""
  ; sort_column = None
  ; sort_dir = Ascending
  ; edit_state = Not_editing
  ; visibility_info = None
  }
