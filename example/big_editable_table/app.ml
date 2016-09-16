open! Core_kernel.Std
open! Incr_dom.Std
open! Js_of_ocaml
open! Splay_tree.Std
open! Async_kernel.Std

open Incr.Let_syntax

module Sort_key = struct
  type t =
    | Int    of int
    | Float  of float
    | String of string
  [@@deriving sexp, compare]
end

module Key = struct
  module T = struct
    type t = Sort_key.t * Row.Id.t [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let id (_, id) : Row.Id.t = id
end

module Model = struct
  type t =
    { rows : Row.Model.t Row.Id.Map.t
    ; column_widths : int list
    ; thead_left : int
    ; cellspacing : int
    ; visible_range : (Key.t * Key.t) option
    ; render_range : (Key.t * Key.t) option
    ; scroll_pos : int * int
    ; focus : Row.Id.t option
    ; edit : Edit.Model.t option
    } [@@deriving sexp_of, fields, compare]

  let empty =
    { rows = Row.Id.Map.empty
    ; column_widths = List.map Row.column_names ~f:(fun _ -> 0)
    ; visible_range = None
    ; render_range = None
    ; focus = None
    ; thead_left = 0
    ; cellspacing = 2
    ; scroll_pos = (0, 0)
    ; edit = None
    }

  let editing t =
    Option.is_some t.edit && Option.is_some t.focus

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Height_splay = struct
  include Splay_tree.Make_with_reduction (Key) (Int)
      (struct
        type key = Key.t
        type data = int
        type accum = int * int
        let identity = (0, 0)
        let singleton ~key:_ ~data = (data, 1)
        let combine (lh, ls) (rh, rs) = (lh + rh, ls + rs)
      end)

  let height t ~spacing =
    let (h, len) = accum t in
    h + len * spacing
end

module Derived_model = struct
  type t =
    { heights : Height_splay.t
    ; rows : Row.Model.t Key.Map.t
    ; get_key : key:Row.Id.t -> data:Row.Model.t -> Key.t
    } [@@deriving sexp_of, fields]

  let create model =
    let get_key =
      Incr.const (fun ~key ~data:_ -> (Sort_key.Int 0, key))
    in
    let folded =
      let%bind get_key = get_key in
      Incr.Map.unordered_fold (model >>| Model.rows)
        ~init:(Height_splay.empty, Key.Map.empty)
        ~f:(fun ~key ~data (heights, rows) ->
          let key = get_key ~key ~data in
          ( Height_splay.set heights ~key ~data:(data.Row.Model.height)
          , Map.add rows ~key ~data
          )
        )
        ~f_inverse:(fun ~key ~data (heights, rows) ->
          let key = get_key ~key ~data in
          ( Height_splay.remove heights key
          , Map.remove rows key
          )
        )
    in
    let%map (heights, rows) = folded
    and     get_key = get_key
    in
    { heights; rows; get_key }
end

module Model_summary = struct
  type t = Model.t

  let create model (_ : Derived_model.t) = model
end

module Action = struct
  type t =
    | Row of (Row.Id.t * Row.Action.t)
    | Focus_row of Row.Id.t
    | Move_edit_focus of [ `Left | `Right ]
    | Move_row_focus of [ `Up | `Down ]
    | Unfocus
    | Edit_action of Edit.Action.t
    | Edit_start
    | Edit_commit
    | Edit_cancel
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = { schedule : Action.t -> unit }
end

let apply_action
      (action:Action.t) (model : Model.t)  (state: State.t)
      ~stabilize_and_get_derived
  =
  let open Option.Let_syntax in
  match action with
  | Row (key, action) ->
    ( match Map.find model.rows key with
      | None -> model
      | Some data ->
        let data = Row.Action.apply action data in
        { model with rows = Map.add model.rows ~key ~data }
    )
  | Focus_row id ->
    if Model.editing model then model
    else { model with focus = Some id }
  | Edit_action action ->
    { model with
      edit =
        let%map edit = model.edit in
        Edit.Action.apply action edit
    }
  | Move_edit_focus dir ->
    if Model.editing model then (
      { model with
        edit =
          let%map edit = model.edit in
          Edit.Action.apply (Move_focus dir) edit
      }
    ) else (
      model
    )
  | Move_row_focus dir ->
    if Model.editing model then (
      model
    ) else (
      let (d : Derived_model.t) = stabilize_and_get_derived () in
      let bound_of_dir = function
        | `Up   -> `Less_than
        | `Down -> `Greater_than
      in
      let next_focus (dir : [ `Up | `Down ]) =
        (* Try to move the focus *)
        let next_focus =
          let%bind focus = model.focus in
          let%bind data = Map.find model.rows focus in
          let key = d.get_key ~key:focus ~data in
          let%map (key, _) = Map.closest_key d.rows (bound_of_dir dir) key in
          Key.id key
        in
        match next_focus, model.focus with
        (* Default to the old focus *)
        | Some focus, _ | None, Some focus -> Some focus
        | None, None ->
          (* With no focus, use the visible range *)
          match (dir, model.visible_range) with
          | (`Up, Some (_, max)) -> Some (Key.id max)
          | (`Down, Some (min, _)) -> Some (Key.id min)
          | _ -> None
      in
      { model with focus = next_focus dir }
    )
  | Unfocus -> state.schedule Edit_cancel; { model with focus = None }
  | Edit_start ->
    if Model.editing model || Option.is_none (Model.focus model) then (
      model
    ) else (
      { model with edit = Some Edit.Model.empty }
    )
  | Edit_commit ->
    ( let%bind edit = model.edit in
      let%bind focus = model.focus in
      let%bind row = Map.find model.rows focus in
      let%map  new_row = Edit.Model.apply edit row in
      { model with rows = Map.add model.rows ~key:focus ~data:new_row; edit = None }
    ) |> Option.value ~default:model
  | Edit_cancel ->
    if Option.is_some model.edit then { model with edit = None } else model

let on_startup ~schedule (m : Model.t) (_ : Derived_model.t) =
  let rows = Map.length m.rows in
  Clock_ns.every (Time_ns.Span.of_ms 50.) (fun () ->
    let row = Row.Id.of_int (Random.int rows) in
    let amount = (Random.int 100) - 50 in
    schedule (Action.Row (row, Change_position amount));
  );
  Clock_ns.every (Time_ns.Span.of_ms 5.) (fun () ->
    let row = Row.Id.of_int (Random.int rows) in
    let price = Random.float 300. in
    schedule (Action.Row (row, Set_price price));
  );
  Deferred.return { State.schedule }
;;

let px_of_int v = (Int.to_string v) ^ "px"
let int_of_px =
  let int_of_float_string s = Float.iround_nearest_exn (Float.of_string s) in
  fun s ->
    (* We expect either "%dpx" or "%d" *)
    match String.substr_index s ~pattern:"px" with
    | None -> int_of_float_string s
    | Some idx ->
      if idx + 2 = String.length s then (
        int_of_float_string (String.slice s 0 idx)
      ) else (
        raise_s [%message "Invalid formatted px" (s : string)]
      )

let getElementById_option id =
  Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))

let find_row_element id =
  getElementById_option (Row.Id.to_dom id)

let at_height heights spacing height =
  Height_splay.search heights
    ~f:(fun ~left:(lh, ls) ~right:_ ->
      let sph = (spacing + 1) / 2 in
      let left = lh + ls * spacing + sph in
      if height < left then `Left else `Right
    )

let first_height heights =
  Height_splay.nth heights 0 |> Option.value_exn

let last_height heights =
  Height_splay.nth heights (Height_splay.length heights - 1) |> Option.value_exn

let table_id = "main-table"
let tbody_id = "main-table-body"
let main_header_id = "main-header"
let floating_header_id = "floating-header"

let compute_heights (m : Model.t) (d : Derived_model.t) =
  let open Option.Let_syntax in
  let new_rows =
    let%map (min, max) = m.visible_range in
    Map.fold_range_inclusive d.rows ~min ~max
      ~init:m.rows
      ~f:(fun ~key ~data rows ->
        let (_, key) = key in
        ( let%map el = find_row_element key in
          let height = Js_misc.viewport_rect_of_element el |> Js_misc.Rect.height in
          if height = data.height then (
            rows
          ) else (
            Map.add rows ~key ~data:{ data with Row.Model.height }
          )
        ) |> Option.value ~default:rows
      )
  in

  match new_rows with
  | None -> m
  | Some rows -> { m with rows }

let latest_nth ~default seq n =
  let before, tail = Sequence.split_n seq n in
  match Sequence.hd tail with
  | Some v -> fst v
  | None ->
    match List.last before with
    | Some v -> fst v
    | None -> default

let nth_after map n key =
  let seq =
    Map.to_sequence map ~keys_greater_or_equal_to:key ~order:`Increasing_key
  in
  latest_nth ~default:key seq n

let nth_before map n key =
  let seq =
    Map.to_sequence map ~keys_less_or_equal_to:key ~order:`Decreasing_key
  in
  latest_nth ~default:key seq n

let update_visibility (model : Model.t) (derived : Derived_model.t) ~recompute_derived =
  let open Option.Let_syntax in

  let (model, derived) =
    let model = compute_heights model derived in
    (model, recompute_derived model)
  in

  let header = Dom_html.getElementById main_header_id in
  let floating_header = getElementById_option floating_header_id in
  let body = Dom_html.getElementById tbody_id in

  let header_rect = Js_misc.viewport_rect_of_element header in
  let body_rect = Js_misc.viewport_rect_of_element body in
  let main_rect = Js_misc.viewport_rect () in

  let open Js_misc.Rect in
  let thead_left = left header_rect in
  let thead_height =
    Option.value_map floating_header
      ~default:(height header_rect)
      ~f:(Fn.compose height Js_misc.viewport_rect_of_element)
  in
  let scroll_top = -(top body_rect) + thead_height in
  let scroll_bot =
    let height = (height main_rect) - thead_height in
    scroll_top + height
  in

  let scroll_pos = (scroll_top, scroll_bot) in

  let column_widths =
    header##querySelectorAll (Js.string "th")
    |> Dom.list_of_nodeList
    |> List.map ~f:(fun el ->
      let style = Dom_html.window##getComputedStyle el in
      int_of_px (Js.to_string style##.width)
    )
  in

  let heights = derived.Derived_model.heights in
  let at_height = at_height heights model.cellspacing in

  let start_key =
    fst @@
    match at_height scroll_top with
    | Some x -> x
    | None -> first_height heights
  in
  let end_key =
    fst @@
    match at_height scroll_bot with
    | Some x -> x
    | None -> last_height heights
  in

  let visible_range =
    Option.some_if
      (scroll_top < Height_splay.height ~spacing:model.cellspacing heights)
      (start_key, end_key)
  in

  let render_range =
    let%map (lo, hi) = visible_range in
    (nth_before derived.rows 10 lo, nth_after derived.rows 10 hi)
  in

  let column_widths =
    if
      List.length column_widths = List.length Row.column_names
      && Option.is_some model.visible_range
    then (
      column_widths
    ) else (
      model.column_widths
    )
  in

  { model with visible_range ; column_widths ; thead_left ; scroll_pos ; render_range }

let view m d ~(inject: Action.t -> Vdom.Event.t) =
  let open Vdom in

  let keydown =
    let%map editing = m >>| Model.editing in
    Attr.on_keydown (fun ev ->
      let to_send =
        match Dom_html.Keyboard_code.of_event ev with
        | ArrowUp    -> inject (Move_row_focus `Up)
        | ArrowDown  -> inject (Move_row_focus `Down)
        | ArrowRight -> inject (Move_edit_focus `Right)
        | ArrowLeft  -> inject (Move_edit_focus `Left)
        | Escape -> inject Edit_cancel
        | Enter  -> inject Edit_commit
        | _ -> Event.Ignore
      in

      let to_send_key =
        if Js.to_bool ev##.ctrlKey || Js.to_bool ev##.altKey then Event.Ignore
        else (
          match Dom_html.Keyboard_code.of_event ev with
          | KeyK -> inject (Move_row_focus `Up   )
          | KeyJ -> inject (Move_row_focus `Down )
          | KeyH -> inject (Move_edit_focus `Left )
          | KeyL -> inject (Move_edit_focus `Right)
          | KeyE ->
            if Js.to_bool ev##.shiftKey then (inject Edit_start)
            else Event.Ignore
          | _ -> Event.Ignore
        )
      in

      let to_send =
        match to_send with
        | Event.Ignore ->
          if not editing then to_send_key else Event.Ignore
        | e -> e
      in

      if to_send <> Event.Ignore then (Event.Many [ to_send; Event.Prevent_default ])
      else Event.Ignore
    )
  in

  let focus = m >>| Model.focus in
  let focused = Staged.unstage (Incr.Select.select_one' (module Row.Id) focus) in

  let visible_range = m >>| Model.render_range in
  let visible_rows =
    Incr.Map.subrange (d >>| Derived_model.rows) visible_range
  in
  let rows =
    Incr.Map.mapi' visible_rows ~f:(fun ~key ~data ->
      let (_, id) = key in
      let focus_me = inject (Focus_row id) in
      let focused = focused id in
      Row.view ~id data ~focus_me ~focused
    )
  in
  let main_header = Row.header (Incr.const [ Attr.id main_header_id ]) in
  let fixed_header =
    let attrs =
      let%map table_left = m >>| Model.thead_left in
      [ Attr.style
          [ "position", "fixed"
          ; "left", px_of_int table_left
          ; "top", "0"
          ; "backgroundColor", "white"
          ]
      ; Attr.id floating_header_id
      ]
    in
    let widths = m >>| Model.column_widths >>| List.map ~f:Option.some in
    Row.header ~widths attrs
  in

  let height_space height =
    Node.div [ Attr.style [ "height", px_of_int height ] ] []
  in

  let cellspacing = m >>| Model.cellspacing in

  let heights =
    let%map visible_range = visible_range
    and     heights = d >>| Derived_model.heights
    and     cellspacing = cellspacing
    in
    let height = Height_splay.height ~spacing:cellspacing in
    match visible_range with
    | None -> (0, height heights)
    | Some (min_key, max_key) ->
      let { Height_splay.Partition.lt; gt; _ } =
        Height_splay.partition heights ~min_key ~max_key
      in
      (height lt, height gt)
  in

  let edit_row =
    let inject action = inject (Edit_action action) in
    let unpack x = Option.value_exn x in
    let%bind editing = m >>| Model.editing in
    if editing then (
      Edit.view (m >>| Model.edit >>| unpack) ~inject >>| Option.some
    ) else (
      Incr.const None
    )
  in

  let headers =
    Incr.if_ (let%map (t, b) = m >>| Model.scroll_pos in t > 0 && b > 0)
      ~then_:(let%map h1 = main_header and h2 = fixed_header in [ h1; h2 ])
      ~else_:(main_header >>| fun x -> [x])
  in

  let%map rows = rows
  and     headers = headers
  and     (before_height, after_height) = heights
  and     cellspacing = cellspacing
  and     edit_row = edit_row
  and     focus = focus
  and     get_key = d >>| Derived_model.get_key
  and     all_data = m >>| Model.rows
  and     keydown = keydown
  in

  let open Option.Let_syntax in

  let (before, at, after) =
    ( let%bind focus = focus in
      let%map data = Map.find all_data focus in
      let key = get_key ~key:focus ~data in
      Map.split rows key
    ) |> Option.value ~default:(rows, None, Key.Map.empty)
  in

  let at_row = Option.map ~f:snd at |> Option.to_list in
  let edit_row = Option.to_list edit_row in

  Node.body [ keydown ]
    [ Node.div [ Attr.style_css "height: 100px;" ] []
    ; Node.table
        [ Attr.id table_id
        ; Attr.style [ "borderSpacing", px_of_int cellspacing ]
        ]
        [ Node.thead [] headers
        ; Node.tbody [ Attr.id tbody_id ]
            (List.concat
               [ [ height_space before_height ]
               ; Map.data before
               ; at_row
               ; edit_row
               ; Map.data after
               ; [ height_space after_height ]
               ])
        ]
    ; Node.div [ Attr.style_css "height: 1000px;" ] []
    ]


let on_display
      ~(old : Model.t)
      (m : Model.t)
      (d : Derived_model.t)
      (state : State.t)
  =
  let open Option.Let_syntax in

  if old.visible_range <> m.visible_range then (
    (* We should invalidate the focus if out of range *)
    let result =
      let%bind focus = m.focus in
      let%bind row = Map.find m.rows focus in
      let key = d.get_key ~key:focus ~data:row in
      let%map (lo, hi) = m.visible_range in
      if Key.( key < lo || key > hi ) then (state.schedule Unfocus)
    in
    ignore (result : unit option)
  ) else if old.focus <> m.focus then (
    let result =
      let%bind focus = m.focus in
      let%bind row = Map.find m.rows focus in

      (* We should scroll to show the focused box *)
      let key = d.Derived_model.get_key ~key:focus ~data:row in

      let before, at, _ = Height_splay.split d.heights key in
      let%map _ = at in

      let start_height = Height_splay.height ~spacing:m.cellspacing before in
      let end_height = start_height + row.Row.Model.height + 2 * m.cellspacing in
      let (scroll_top, scroll_bot) = m.scroll_pos in

      if start_height < scroll_top then (
        let dif = scroll_top - start_height in
        Dom_html.window##scrollBy 0 (-dif)
      ) else if end_height > scroll_bot then (
        let dif = end_height - scroll_bot in
        Dom_html.window##scrollBy 0 dif
      )
    in
    ignore (result : unit option)
  );

  Option.iter m.edit ~f:(fun edit ->
    let set_edit_focus =
      match old.edit with
      | None -> true
      | Some old_edit -> edit.focus <> old_edit.focus
    in
    if set_edit_focus then (
      let input =
        Dom_html.getElementById (Edit.Field.to_dom edit.focus)
        |> Dom_html.CoerceTo.input
      in
      Js.Opt.iter input (fun el -> el##focus)
    )
  )
;;

let init () =
  let rows =
    List.init 10000 ~f:(fun idx ->
      (Row.Id.of_string (Int.to_string idx), Row.Model.empty))
  in
  { Model.empty with
    rows = Row.Id.Map.of_alist_exn rows
  }
