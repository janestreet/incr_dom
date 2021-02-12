open! Core_kernel
open! Incr_dom
open! Js_of_ocaml
open! Async_kernel
open Incr_dom_partial_render
open Incr.Let_syntax
module Measurements = Partial_render_list.Measurements

module Key : sig
  type sort =
    | Num
    | Native
  [@@deriving sexp, compare]

  type t [@@deriving sexp]

  val create : sort -> Row.Id.t -> t
  val id : t -> Row.Id.t

  include Comparable with type t := t
end = struct
  module T = struct
    type sort =
      | Num
      | Native
    [@@deriving sexp, compare, equal]

    type t = sort * Row.Id.t [@@deriving sexp]

    let compare (sort, row_id) (sort', row_id') =
      if not ([%equal: sort] sort sort')
      then compare_sort sort sort'
      else (
        match sort with
        | Num ->
          let to_int x = Int.of_string (Row.Id.to_string x) in
          Int.compare (to_int row_id) (to_int row_id')
        | Native -> Row.Id.compare row_id row_id')
    ;;
  end

  let create sort id = sort, id
  let id (_, id) = id

  include T
  include Comparable.Make (T)
end

module Row_view = Partial_render_list.Make_simple (Key)

module Model = struct
  type t =
    { rows : Row.Model.t Row.Id.Map.t
    ; filter_string : string
    ; sort : Key.sort
    ; height_cache : Row_view.Height_cache.t
    ; measurements : Measurements.t option
    }
  [@@deriving sexp_of, fields, compare]

  let create size =
    { filter_string = ""
    ; sort = Key.Num
    ; rows =
        Row.Id.Map.of_alist_exn
          (List.init size ~f:(fun id ->
             ( Row.Id.of_string (Int.to_string id)
             , Row.Model.create ~data:(Int.to_string id) )))
    ; height_cache = Row_view.Height_cache.empty ~height_guess:18.
    ; measurements = None
    }
  ;;

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Change_row of (Row.Id.t * Row.Action.t)
    | Bump_row_height
    | Update_filter of string
    | Update_sort of Key.sort
  [@@deriving sexp_of]
end

module State = struct
  type t = unit
end

let apply_action (model : Model.t) (action : Action.t) _ ~schedule_action:_ =
  match action with
  | Change_row (key, action) ->
    (match Map.find model.rows key with
     | None -> model
     | Some row ->
       { model with rows = Map.set model.rows ~key ~data:(Row.Action.apply action row) })
  | Bump_row_height ->
    (* Hack to find random key, for testing *)
    let key = Row.Id.of_string @@ Int.to_string @@ Random.int (Map.length model.rows) in
    (match Map.find model.rows key with
     | None -> model
     | Some row ->
       { model with
         rows =
           Map.set
             model.rows
             ~key
             ~data:(Row.Action.apply (Row.Action.Increase_font_by 10) row)
       })
  | Update_filter filter_string -> { model with filter_string }
  | Update_sort sort -> { model with sort }
;;

let on_startup ~schedule_action _model =
  Clock_ns.every (Time_ns.Span.of_ms 50.) (fun () ->
    schedule_action Action.Bump_row_height);
  Deferred.return ()
;;

let update_visibility (model : Model.t) row_view ~schedule_action:_ =
  let height_cache =
    Row_view.measure_heights_simple row_view ~measure:(fun key ->
      let open Option.Let_syntax in
      let id = Row.Id.to_string (Key.id key) in
      let%map elt = Dom_html.getElementById_opt id in
      let rect = Js_misc.viewport_rect_of_element elt in
      rect.bottom -. rect.top)
  in
  let measurements =
    let open Option.Let_syntax in
    let%map table_container = Dom_html.getElementById_opt "table-container"
    and table_body = Dom_html.getElementById_opt "table-body" in
    { Measurements.view_rect = Js_misc.client_rect_of_element table_container
    ; list_rect = Js_misc.client_rect_of_element table_body
    }
  in
  if
    [%compare.equal: Row_view.Height_cache.t] height_cache model.height_cache
    && [%compare.equal: Measurements.t option] measurements model.measurements
  then model
  else { model with height_cache; measurements }
;;

let view model row_view ~inject =
  let scroll_attr = Vdom.Attr.on_scroll (fun _ -> Vdom.Event.Viewport_changed) in
  let filter_string_change =
    Vdom.Attr.on_input (fun (_ : Dom_html.event Js.t) value ->
      inject (Action.Update_filter value))
  in
  let sort_change =
    Vdom.Attr.on_change (fun (_ : Dom_html.event Js.t) value ->
      match value with
      | "Num" -> inject (Action.Update_sort Key.Num)
      | "Native" -> inject (Action.Update_sort Key.Native)
      | _ -> Vdom.Event.Ignore)
  in
  let offset_div key height =
    Vdom.Node.div
      ~key
      [ Vdom.Attr.style (Css_gen.height (`Px (Float.iround_nearest_exn height))) ]
      []
  in
  let visible_rows = row_view >>| Row_view.rows_to_render in
  let visible_rows_dom =
    Incr.Map.mapi' visible_rows ~f:(fun ~key ~data ->
      let key = Key.id key in
      let inject act = inject (Action.Change_row (key, act)) in
      Row.view ~inject ~row_id:key data)
  in
  let%map visible_rows_dom = visible_rows_dom
  and filter_string = model >>| Model.filter_string
  and start_height, end_height = Row_view.spacer_heights row_view in
  let start_offset = offset_div "start_offset" start_height in
  let end_offset = offset_div "end_offset" end_height in
  let open Vdom in
  Node.body
    []
    [ Node.div
        [ Attr.class_ "header" ]
        [ Node.div
            [ Attr.id "text-input" ]
            [ Node.input
                [ Attr.type_ "text"; Attr.value filter_string; filter_string_change ]
                []
            ; Node.select
                [ sort_change ]
                [ Node.option [ Attr.value "Num" ] [ Node.text "Numeric" ]
                ; Node.option [ Attr.value "Native" ] [ Node.text "Lexicographic" ]
                ]
            ; Node.div
                []
                [ Node.text "Add ?number to the URL to change the number of rows" ]
            ]
        ]
    ; Node.div
        [ scroll_attr; Attr.id "table-container" ]
        [ Node.div
            [ Attr.id "table-body" ]
            (start_offset :: Map.data visible_rows_dom @ [ end_offset ])
        ]
    ]
;;

let create_row_view (model : Model.t Incr.t) =
  let rows = model >>| Model.rows in
  let%bind filter_string = model >>| Model.filter_string
  and sort = model >>| Model.sort in
  let rows =
    Incr.Map.unordered_fold
      rows
      ~init:Key.Map.empty
      ~add:(fun ~key ~(data : Row.Model.t) m ->
        if String.is_substring data.data ~substring:filter_string
        then (
          let key = Key.create sort key in
          Map.set m ~key ~data)
        else m)
      ~remove:(fun ~key ~data:_ m ->
        let key = Key.create sort key in
        Map.remove m key)
  in
  let height_cache = model >>| Model.height_cache in
  let measurements = model >>| Model.measurements in
  Row_view.create ~rows ~height_cache ~measurements
;;

let create model ~old_model:_ ~inject =
  let row_view = create_row_view model in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and update_visibility =
    let%map model = model
    and row_view = row_view in
    update_visibility model row_view
  and view = view model row_view ~inject
  and model = model in
  Component.create ~apply_action ~update_visibility model view
;;
