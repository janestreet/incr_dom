open! Async_kernel
open! Core
open! Import
open Js_of_ocaml

let log_s = Async_js.log_s

module Model = struct
  type t =
    { entries : Entry.Model.t Entry_id.Map.t
    ; focus : (Entry_id.t * Focus_point.t option) option
    ; search_string : string
    ; visible_range : (Entry_id.t * Entry_id.t) option
    }
  [@@deriving fields, sexp, compare, equal]

  let name_found_by_search ~search_string name =
    String.( = ) search_string ""
    || String.is_prefix (String.lowercase name) ~prefix:(String.lowercase search_string)
  ;;

  let filtered_entries t =
    Map.filteri t.entries ~f:(fun ~key:_ ~data:e ->
      name_found_by_search ~search_string:t.search_string (Entry.Model.name e))
  ;;

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Entry of (Entry_id.t * Focus_point.t option) option * Entry.Action.t
    | Set_outer_focus of Entry_id.t
    | Set_inner_focus of Focus_point.t
    | Move_outer_focus of focus_dir
    | Move_inner_focus of focus_dir
    | Set_search_string of string
    | Raise of Error.t
    | Raise_js
    | Nop
    | Dump_state
    | Kick_all
    | Kick_n of int
  [@@deriving sexp]

  let kick_n n = Kick_n n
end

module State = struct
  type t = unit
end

let do_kick_all (model : Model.t) =
  if Float.( < ) (Random.float 1.) 0.6
  then model
  else (
    let entries = Map.map model.entries ~f:Entry.Model.kick in
    { model with entries })
;;

let do_kick_one (model : Model.t) =
  let pos = Random.int (Map.length model.entries) in
  match Map.nth model.entries pos with
  | None -> model
  | Some (key, _) ->
    let entries =
      Map.change model.entries key ~f:(function
        | None -> None
        | Some x -> Some (Entry.Model.kick x))
    in
    { model with entries }
;;

let do_kick_n model n =
  Sequence.fold ~init:model (Sequence.range 0 n) ~f:(fun old (_ : int) -> do_kick_one old)
;;

let entry_apply (m : Model.t) entry_id focus_point action =
  let entries =
    Map.change m.entries entry_id ~f:(function
      | None -> None
      | Some m -> Some (Entry.Action.apply action focus_point m))
  in
  { m with entries }
;;

let move_inner_focus (dir : focus_dir) (m : Model.t) =
  let focus =
    match m.focus with
    | None -> None
    | Some (entry_id, fp) ->
      (match Map.find m.entries entry_id with
       | None -> None
       | Some entry ->
         let fp = Entry.Model.move_focus entry fp dir in
         Some (entry_id, fp))
  in
  { m with focus }
;;

let move_outer_focus dir (m : Model.t) =
  let outer, inner =
    match m.focus with
    | None -> None, None
    | Some (outer, inner) -> Some outer, inner
  in
  match move_map_focus (Model.filtered_entries m) outer dir with
  | None -> { m with focus = None }
  | Some new_outer -> { m with focus = Some (new_outer, inner) }
;;

let set_outer_focus entry_id (m : Model.t) =
  let focus =
    match m.focus with
    | None -> Some (entry_id, None)
    | Some (_, focus_point) -> Some (entry_id, focus_point)
  in
  { m with focus }
;;

let set_inner_focus fp (m : Model.t) =
  let focus =
    match m.focus with
    | None -> None
    | Some (entry_id, _) -> Some (entry_id, Some fp)
  in
  { m with focus }
;;

let apply_action (m : Model.t) action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Move_outer_focus dir -> move_outer_focus dir m
  | Move_inner_focus dir -> move_inner_focus dir m
  | Set_outer_focus entry_id -> set_outer_focus entry_id m
  | Set_inner_focus fp -> set_inner_focus fp m
  | Kick_all -> do_kick_all m
  | Kick_n n -> do_kick_n m n
  | Entry (focus, action) ->
    (match focus with
     | None -> m
     | Some (entry_id, focus_point) -> entry_apply m entry_id focus_point action)
  | Raise err -> Error.raise err
  | Raise_js -> Js_of_ocaml.Js.Unsafe.js_expr "xxxxxxxxxxxxx.yy()"
  | Dump_state ->
    log_s [%message "Model" (m : Model.t)];
    m
  | Nop -> m
  | Set_search_string search_string -> { m with search_string }
;;

let on_startup ~schedule_action (_ : Model.t) : State.t Deferred.t =
  Js_misc.scroll ();
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action (Action.kick_n 150));
  return ()
;;

let in_range cmp (lo, hi) value = cmp lo value <= 0 && cmp hi value >= 0

let view (m : Model.t Incr.t) ~inject =
  let open Vdom in
  let open Incr.Let_syntax in
  let set_inner_focus fp = inject (Action.Set_inner_focus fp) in
  let focus = m >>| Model.focus in
  let on_keydown =
    let%map focus = focus in
    Attr.on_keydown (fun ev ->
      match Dom_html.Keyboard_code.of_event ev with
      | KeyK -> inject (Move_outer_focus Prev)
      | KeyJ -> inject (Move_outer_focus Next)
      | KeyU -> inject (Move_inner_focus Prev)
      | KeyI -> inject (Move_inner_focus Next)
      | KeyX ->
        if Js.to_bool ev##.ctrlKey
        then inject (Raise (Error.of_string "got X"))
        else Vdom.Event.Ignore
      | KeyY -> if Js.to_bool ev##.ctrlKey then inject Raise_js else Vdom.Event.Ignore
      | KeyD -> inject Nop
      | KeyS -> inject Dump_state
      | KeyE -> inject (Entry (focus, Toggle_collapse))
      | Equal -> inject (Entry (focus, Bump Incr))
      | Minus -> inject (Entry (focus, Bump Decr))
      | _ -> Vdom.Event.Ignore)
  in
  (* Right now, the incrementality of this is terrible.  Waiting on better support from
     Incremental. *)
  let input =
    Node.input
      ~attr:
        (Attr.many_without_merge
           [ Attr.create "type" "text"
           ; Attr.on_input (fun _ev text -> inject (Set_search_string text))
           ])
      []
  in
  let entries = m >>| Model.entries in
  let search_string = m >>| Model.search_string in
  let visible_range = m >>| Model.visible_range in
  let%map entries =
    Incr.Map.filter_mapi' entries ~f:(fun ~key:entry_id ~data:entry ->
      log_s [%message "creating entry" (entry_id : Entry_id.t)];
      let name = entry >>| Entry.Model.name in
      let visible =
        match%map visible_range with
        | None -> false
        | Some range -> in_range Entry_id.compare range entry_id
      in
      let%bind name = name
      and search_string = search_string in
      if not (Model.name_found_by_search ~search_string name)
      then Incr.const None
      else (
        let focus_me = inject (Action.Set_outer_focus entry_id) in
        let focus =
          match%map focus with
          | None -> Entry.Unfocused
          | Some (entry_id', fp) ->
            if Entry_id.( = ) entry_id entry_id'
            then Entry.Focused fp
            else Entry.Unfocused
        in
        let%map view =
          Entry.view entry entry_id ~visible ~focus ~focus_me ~set_inner_focus
        in
        Some view))
  and on_keydown = on_keydown in
  Node.body ~attr:on_keydown (input :: Map.data entries)
;;

let update_visibility m ~schedule_action:_ =
  let filtered_entries = Model.filtered_entries m in
  let visible_range =
    Js_misc.find_visible_range
      ~length:(Map.length filtered_entries)
      ~nth_element_id:(fun n ->
        Map.nth_exn filtered_entries n |> fst |> Entry_id.id_string)
      Js_misc.Rows
    |> Option.map ~f:(fun (a, b) ->
      Map.nth_exn filtered_entries a |> fst, Map.nth_exn filtered_entries b |> fst)
  in
  { m with visible_range }
;;

let example ~entries : Model.t =
  let entries =
    List.init entries ~f:(fun _ -> Entry.Model.example ())
    |> List.mapi ~f:(fun i x -> Entry_id.create i, x)
    |> Entry_id.Map.of_alist_exn
  in
  let focus =
    match Map.min_elt entries with
    | Some (k, _) -> Some (k, None)
    | None -> None
  in
  { focus; entries; search_string = ""; visible_range = None }
;;

let on_display ~(old_model : Model.t) (model : Model.t) _ ~schedule_action:_ =
  let get_focus (m : Model.t) = Option.map m.focus ~f:fst in
  if not ([%equal: Entry_id.t option] (get_focus old_model) (get_focus model))
  then Js_misc.scroll ()
;;

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and update_visibility =
    let%map model = model in
    update_visibility model
  and on_display =
    let%map old_model = old_model
    and model = model in
    on_display ~old_model model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action ~update_visibility ~on_display model view
;;
