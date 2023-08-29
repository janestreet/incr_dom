open Core
open Incr_dom
open Incr.Let_syntax

module Row = struct
  module Id = struct
    include Unique_id.Int ()
  end

  type t =
    { id : Id.t
    ; field1 : string
    ; field2 : float
    }
  [@@deriving fields ~getters, compare]
end

module T =
  Incr_dom_partial_render.Table.Make (Row.Id) (Int)
    (Incr_dom_partial_render.Table.Default_sort_spec)

module Model = struct
  type t =
    { rows : Row.t Row.Id.Map.t
    ; table : T.Model.t
    }
  [@@deriving fields ~getters, compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t = Table_action of T.Action.t [@@deriving sexp]
end

module State = struct
  type t = unit
end

let apply_action table (model : Model.t Incr.t) =
  let%map model = model
  and table_apply_action = table >>| Component.apply_action in
  fun (action : Action.t) _state ~schedule_action ->
    let schedule_table_action action = schedule_action (Action.Table_action action) in
    let apply_table_action action =
      table_apply_action action () ~schedule_action:schedule_table_action
    in
    match action with
    | Table_action a -> { model with table = apply_table_action a }
;;

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let columns =
  let open Vdom in
  let open Incr_dom_partial_render.Table.Default_sort_spec in
  let column ?sort_by header =
    T.Column.create ?sort_by ~header:(Node.span [ Node.text header ]) ()
  in
  [ ( 0
    , column ~sort_by:(fun _ row -> Sort_key.String (Row.field1 row)) "Field1"
    , fun row -> Row.field1 row )
  ; ( 1
    , column ~sort_by:(fun _ row -> Sort_key.Float (Row.field2 row)) "Field2"
    , fun row -> Row.field2 row |> fun s -> sprintf "%.2f%%" (100. *. s) )
  ]
;;

module Rn_spec = Incr_dom_partial_render.Row_node_spec

let row_view (row : Row.t Incr.t) =
  let open Vdom in
  let%bind row = row in
  let text_cell text = { Rn_spec.Cell.attrs = []; nodes = [ Node.text text ] } in
  let cells =
    List.map columns ~f:(fun (_index, _col, to_string) -> text_cell (to_string row))
  in
  return { Rn_spec.row_attrs = []; cells }
;;

let row_renderer (_m : Model.t Incr.t) ~inject:_ : Row.t T.row_renderer =
  fun ~row_id:_ ~row -> row_view row
;;

let height_guess = 43.

(* The [on_display] part of the [Component.t] record takes the [old_model] alongside the
   new [model] so that it can do more intelligent diffing (and perform side-effectful
   imperative actions based on that diff).

   In the case of [Table], it looks at whether row focus changes -- if so, it scrolls the
   browser to look at the row that just got focus (a feature not used in this example).
*)
let create_table_component (model : Model.t Incr.t) ~old_model ~inject =
  let rows = Incr.map model ~f:(fun m -> Model.rows m) in
  let render_row = row_renderer model ~inject in
  let table_model = model >>| Model.table in
  let old_table_model = old_model >>| Model.table >>| Option.some in
  T.create
    table_model
    ~old_model:old_table_model
    ~rows
    ~columns:(List.map columns ~f:(fun (i, c, _) -> i, c) |> Incr.const)
    ~render_row
    ~inject:(fun a -> inject (Action.Table_action a))
    ~attrs:[ Vdom.Attr.classes [ "table"; "table-bordered" ] ]
;;

let generate_fake_data () =
  let generator =
    List.gen_with_length
      1_000
      (Quickcheck.Generator.tuple2
         (String.gen_with_length 10 Quickcheck.Generator.char_alpha)
         (Float.gen_incl 0. 1.))
  in
  let values =
    Quickcheck.Generator.generate
      generator
      ~size:1_000
      ~random:(Splittable_random.State.of_int 1)
  in
  List.map values ~f:(fun tup ->
    let id = Row.Id.create () in
    id, { Row.id; field1 = fst tup; field2 = snd tup })
;;

let init () : Model.t =
  let table =
    T.Model.create
      ~initial_sort:[ { column = 0; dir = Ascending } ]
      ~scroll_margin:(Incr_dom_partial_render.Table.Margin.uniform 5.)
      ~scroll_region:(Element "table-container")
      ~float_header:Edge
      ~float_first_col:(Px_from_edge (-1))
      ~height_guess
      ()
  in
  let rows = Row.Id.Map.of_alist_exn (generate_fake_data ()) in
  { rows; table }
;;

let view table (_m : Model.t Incr.t) ~inject:_ =
  let open Vdom in
  let scroll_attr = Vdom.Attr.on_scroll (fun _ -> Vdom.Effect.Viewport_changed) in
  let%map table = table >>| Component.view in
  Node.div
    ~attrs:[ Attr.id "app" ]
    [ Jane_web_style.Css.style_4
    ; Node.div
        ~key:"table"
        ~attrs:[ Attr.many_without_merge [ Attr.id "table-container"; scroll_attr ] ]
        [ table ]
    ]
;;

let update_visibility table (m : Model.t Incr.t) =
  let%map m = m
  and table = table in
  fun ~schedule_action ->
    let table =
      Component.update_visibility table ~schedule_action:(fun a ->
        schedule_action (Action.Table_action a))
    in
    { m with table }
;;

let create model ~old_model ~inject =
  let table = create_table_component model ~old_model ~inject in
  let%map apply_action = apply_action table model
  and update_visibility = update_visibility table model
  and view = view table model ~inject
  and model = model in
  Component.create ~apply_action ~update_visibility model view
;;
