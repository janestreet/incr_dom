open! Core_kernel
open! Incr_dom
open! Js_of_ocaml
open! Async_kernel.Std
open Splay_tree.Std

(* This module is a simple implementation of partial rendering, which incrementally
   computes the set of DOM nodes in view, along with a small window around the edge.  This
   works by keeping track of the heights of all elements in a splay tree that allows us to
   query for nodes by height. We assume a default height for any node that has never been
   rendered.

   Note that this idea has been more fully realized by some reusable modules in the
   [Incr_dom_widgets] library *)

open Incr.Let_syntax

module Key : sig
  type sort = Num | Native [@@deriving sexp, compare]

  type t [@@deriving sexp]

  val create : sort -> Row.Id.t -> t
  val id : t -> Row.Id.t

  include Comparable with type t := t
end = struct
  module T = struct
    type sort = Num | Native [@@deriving sexp, compare]
    type t = sort * Row.Id.t [@@deriving sexp]

    let compare (sort, row_id) (sort', row_id') =
      if sort <> sort'
      then compare_sort sort sort'
      else match sort with
        | Num ->
          let to_int x = Int.of_string (Row.Id.to_string x) in
          Int.compare (to_int row_id) (to_int row_id')
        | Native -> Row.Id.compare row_id row_id'
  end

  let create sort id = (sort, id)
  let id (_, id) = id

  include T
  include Comparable.Make(T)
end

module Model = struct
  type t =
    { visible_range : (Key.t * Key.t) option
    ; rows : Row.Model.t Row.Id.Map.t
    ; filter_string : string
    ; sort : Key.sort
    } [@@deriving sexp_of, fields, compare]

  let create size =
    { visible_range = None
    ; filter_string = ""
    ; sort = Key.Num
    ; rows =
        Row.Id.Map.of_alist_exn
          (List.init size ~f:(fun id ->
             ( Row.Id.of_string (Int.to_string id)
             , Row.Model.create ~data:(Int.to_string id) ~height:18
             )
           ))
    }

  let cutoff t1 t2 = compare t1 t2 = 0
end

(* We keep a map from each row to its height in a splay tree to efficiently calculate the
   total height of rows that are not shown on the screen. *)
module Heights =
  Splay_tree.Make_with_reduction (Key) (Int)
    (struct
      type key = Key.t
      type data = int
      type accum = int
      let identity = 0
      let singleton ~key:_ ~data = data
      let combine = (+)
    end)

module Derived_model = struct
  type t =
    { rows : Row.Model.t Key.Map.t
    ; heights : Heights.t
    } [@@deriving sexp_of, fields]

  let create (model : Model.t Incr.t) =
    let rows = model >>| Model.rows in
    let%bind filter_string = model >>| Model.filter_string
    and      sort = model >>| Model.sort
    in
    let%map rows, heights =
      Incr.Map.unordered_fold rows ~init:(Key.Map.empty, Heights.empty)
        ~f:(fun ~key ~(data : Row.Model.t) (m, h) ->
          if String.is_substring data.data ~substring:filter_string then
            let key = Key.create sort key in
            ( Map.add m ~key ~data
            , Heights.set h ~key ~data:(Row.Model.height data)
            )
          else (m, h)
        )
        ~f_inverse:(fun ~key ~data:_ (m, h) ->
          let key = Key.create sort key in
          ( Map.remove m key
          , Heights.remove h key
          )
        )
    in
    { rows; heights }
end

module Model_summary = struct
  type t = Model.t

  let create m _ = m
end

module Action = struct
  type t =
    | Change_row of (Row.Id.t * Row.Action.t)
    | Bump_row_height
    | Update_filter of string
    | Update_sort of Key.sort
    [@@deriving sexp_of]

  let should_log = function
    | Bump_row_height -> false
    | Change_row (_, action) -> Row.Action.should_log action
    | Update_filter _ | Update_sort _ -> true
end

module State = struct type t = unit end

let apply_action
      (action:Action.t) (model : Model.t) (_:State.t)
      ~recompute_derived:_
  =
  match action with
  | Change_row (key, action) ->
    ( match Map.find model.rows key with
      | None -> model
      | Some row ->
        { model with
          rows = Map.add model.rows ~key ~data:(Row.Action.apply action row)
        }
    )
  | Bump_row_height ->
    (* Hack to find random key, for testing *)
    let key =
      Row.Id.of_string @@ Int.to_string @@ Random.int (Map.length model.rows)
    in
    ( match Map.find model.rows key with
      | None -> model
      | Some row ->
        { model with
          rows =
            Map.add model.rows
              ~key
              ~data:(Row.Action.apply (Row.Action.Increase_font_by 10) row)
        }
    )
  | Update_filter filter_string -> { model with filter_string }
  | Update_sort   sort          -> { model with sort }


let on_startup ~schedule _model _derived =
  Clock_ns.every (Time_ns.Span.of_ms 50.) (fun () ->
    schedule Action.Bump_row_height
  );
  Deferred.return ()

(* Finds the last node for which the total height is at most [value]. *)
let search_height heights value =
  Heights.search heights
    ~f:(fun ~left ~right:_ ->
      if value < left then `Left else `Right
    )

let update_visibility (model : Model.t) (derived : Derived_model.t) ~recompute_derived =
  let (model, derived) =
    match model.visible_range with
    | None -> (model, derived)
    | Some (min, max) ->
      let rows =
        Map.fold_range_inclusive derived.rows ~min ~max ~init:model.rows
          ~f:(fun ~key ~data rows ->
            let key = Key.id key in
            let height =
              let open Option.Let_syntax in
              let id = Row.Id.to_string key in
              let%map elt =
                Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))
              in
              let rect = Js_misc.viewport_rect_of_element elt in
              rect.bottom - rect.top
            in
            let height = Option.value ~default:data.height height in
            if height <> data.height
            then Map.add rows ~key ~data:{ data with height }
            else rows
          )
      in
      let model = { model with rows } in
      (model, recompute_derived model)
  in

  let container = Dom_html.getElementById "table-container" in
  let top = container##.scrollTop in
  let height = container##.clientHeight in
  let bottom = top + height in

  let heights = derived.heights in

  let visible_range =
    let start_key =
      match search_height heights top with
      | None -> Heights.remove_min heights |> Option.map ~f:Tuple3.get1
      | Some (key,_) -> Some key
    in
    let end_key =
      match search_height heights bottom with
      | None -> Heights.remove_max heights |> Option.map ~f:Tuple3.get1
      | Some (key,_) -> Some key
    in
    match start_key,end_key with
    | None,_ | _, None -> None
    | Some x, Some y -> Some (x,y)
  in
  { model with visible_range }

let view model derived ~inject =
  let scroll_attr = Vdom.Attr.on "scroll" (fun _ -> Vdom.Event.Viewport_changed) in
  let filter_string_change =
    Vdom.Attr.on_input (fun (_ : Dom_html.event Js.t) value ->
      inject (Action.Update_filter value))
  in
  let sort_change =
    Vdom.Attr.on_change (fun (_ : Dom_html.event Js.t) value ->
      match value with
      | "Num" -> inject (Action.Update_sort Key.Num)
      | "Native" -> inject (Action.Update_sort Key.Native)
      | _ -> Vdom.Event.Ignore
    )
  in

  let offset_div key height =
    Vdom.Node.div ~key
      [ Vdom.Attr.style [ "height", sprintf "%dpx" height ]
      ]
      []
  in

  let rows = derived >>| Derived_model.rows in
  let visible_range = model >>| Model.visible_range in

  let visible_rows = Incr.Map.subrange rows visible_range in
  let visible_rows_dom =
    Incr.Map.mapi' visible_rows
      ~f:(fun ~key ~data ->
        let key = Key.id key in
        let inject act = inject (Action.Change_row (key, act)) in
        Row.view ~inject ~row_id:key data
      )
  in

  let%map visible_range = visible_range
  and     heights = derived >>| Derived_model.heights
  and     visible_rows_dom = visible_rows_dom
  and     filter_string = model >>| Model.filter_string
  in

  let (start_height, end_height) =
    match visible_range with
    | None -> (0, Heights.accum heights)
    | Some (min, max) ->
      let { Heights.Partition. lt; gt; _ } =
        Heights.partition ~min_key:min ~max_key:max heights
      in
      ( Heights.accum lt, Heights.accum gt)
  in

  let start_offset = offset_div "start_offset" start_height in
  let end_offset = offset_div "end_offset" end_height in

  Vdom.Node.body []
    [ Vdom.Node.div [ Vdom.Attr.class_ "header" ]
        [ Vdom.Node.div [Vdom.Attr.id "text-input"] [
            Vdom.Node.input
              [ Vdom.Attr.type_ "text"
              ; Vdom.Attr.value filter_string
              ; filter_string_change
              ] []
          ; Vdom.Node.select [ sort_change ]
              [ Vdom.Node.option [ Vdom.Attr.value "Num" ]
                  [ Vdom.Node.text "Numeric" ]
              ; Vdom.Node.option [ Vdom.Attr.value "Native" ]
                  [ Vdom.Node.text "Lexicographic" ]
              ]
          ; Vdom.Node.div []
              [ Vdom.Node.text "Add ?number to the URL to change the number of rows" ]
          ]
        ]
    ; Vdom.Node.div
        [ scroll_attr
        ; Vdom.Attr.id "table-container"
        ]
        (start_offset :: (Map.data visible_rows_dom) @ [end_offset])
    ]
;;

let on_display ~old:_ _new_model _new_derived _state = ()
