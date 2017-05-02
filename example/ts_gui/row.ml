module My_time = Time
open! Core_kernel
open! Import
module Time = My_time

module Rn_spec = Row_node_spec

module Model = struct
  type t =
    { symbol    : string
    ; edge      : float
    ; max_edge  : float
    ; bsize     : int
    ; bid       : float
    ; ask       : float
    ; asize     : int
    ; position  : int
    ; last_fill : Time.t option
    ; trader    : string
    }
  [@@deriving compare, fields]

  let columns =
    let append f list field = f field :: list in
    let add ?group ?(editable=false) ?focus_on_edit ?sort_by m =
      append
        (fun field -> Column.of_field field m ~editable ?group ?sort_by ?focus_on_edit)
    in
    let num_f x = Sort_key.Float x in
    let num_i x ~f = Sort_key.Float (Float.of_int (f x)) in
    let num_i = num_i ~f:Fn.id in
    let time t =
      Sort_key.Float (
        Option.value ~default:Time_ns.epoch t
        |> Time_ns.to_int63_ns_since_epoch
        |> Int63.to_int64 |> Float.of_int64)
    in
    let lex_s x = Sort_key.String x in
    let module Time_opt = struct
      type t = Time.t option
      let to_string = function
        | None -> "-"
        | Some x -> Time.to_string x
      let of_string = function
        | "-" -> None
        | s -> Time.of_string s
    end in
    let edge_group = "Edge data" in
    let market_group = "Market data" in
    let trade_group = "Trade data" in
    Fields.fold ~init:[]
      ~symbol:   (add (module String) ~sort_by:lex_s)
      ~edge:     (add (module Float) ~group:edge_group ~editable:true ~focus_on_edit:()
                    ~sort_by:num_f)
      ~max_edge: (add (module Float) ~group:edge_group ~editable:true ~sort_by:num_f)
      ~trader:   (add (module String) ~group:trade_group ~editable:true ~sort_by:lex_s)
      ~bsize:    (add (module Int) ~group:market_group ~sort_by:num_i)
      ~bid:      (add (module Float) ~group:market_group ~sort_by:num_f)
      ~ask:      (add (module Float) ~group:market_group ~sort_by:num_f)
      ~asize:    (add (module Int) ~group:market_group ~sort_by:num_i)
      ~position: (add (module Int) ~group:trade_group ~sort_by:num_i)
      ~last_fill:(add (module Time_opt) ~group:trade_group ~sort_by:time)
    |> List.rev

  let matches_pattern t pattern =
    let matches s =
      String.is_substring ~substring:pattern
        (String.lowercase s)
    in
    matches t.symbol

  let apply_edit t ~column value =
    match List.find columns ~f:(fun col -> Column.name col = column) with
    | None -> t
    | Some column ->
      match Column.set column t value with
      | Error _ -> t
      | Ok t' -> t'

end

module Action = struct
  type t =
    | Kick_price
    | Kick_position
  [@@deriving sexp]

  let kick_price = Kick_price
  let kick_position = Kick_position
end

let kick_price (m:Model.t) =
  let move = Float.of_int (Random.int 15 - 7) /. 100. in
  let spread = m.ask -. m.bid in
  let bid = Float.max 0.20 (m.bid +. move) in
  let ask = bid +. spread in
  { m with bid; ask }

let kick_position (m:Model.t) =
  let position = m.position + 10 * (Random.int 21 - 10) in
  { m with position; last_fill = Some (Time.now ()) }

let apply_action (action : Action.t) (m:Model.t) =
  match action with
  | Kick_price     -> kick_price m
  | Kick_position -> kick_position m

module Mode = struct
  type t = Unfocused | Focused | Editing
  [@@deriving sexp]
end

let editable_cell m col ~remember_edit =
  let open Vdom in
  let attrs =
    [ Attr.style [ "width", "100%" ]
    ; Attr.create "size" "1"
    ; Attr.value (Column.get col m)
    ; Attr.on_input (fun _ value ->
        remember_edit ~column:(Column.name col) value)
    ]
    @ (if Column.focus_on_edit col
       then [ Attr.id "focus-on-edit" ]
       else [])
  in
  Node.input attrs []
;;

let column_cell m col ~editing ~remember_edit =
  let open Vdom in
  if editing && Column.editable col
  then (editable_cell m col ~remember_edit)
  else (Node.span [] [Node.text (Column.get col m)])
;;

let view
      (m:Model.t Incr.t)
      ~(mode: Mode.t Incr.t)
      ~sort_column
      ~focused_column
      ~focus_me
      ~focus_nth_column
      ~remember_edit
  =
  let open Vdom in
  let on_click = Attr.on_click (fun _ -> focus_me) in
  let style =
    let%bind last_fill = m >>| Model.last_fill in
    match last_fill with
    | None -> Incr.const None
    | Some last_fill ->
      let start_fading = Time_ns.add last_fill (Time_ns.Span.of_sec 1.0) in
      let end_fading   = Time_ns.add start_fading (Time_ns.Span.of_sec 1.0) in
      Incr.step_function ~init:(Some "new")
        [ start_fading, Some "fading"
        ; end_fading, None
        ]
  in
  let%map m = m and mode = mode and style = style and focused_column = focused_column in
  let focused_attr =
    match mode with
    | Focused | Editing -> [Attr.class_ "row-focused"]
    | Unfocused -> []
  in
  let editing =
    match mode with
    | Editing -> true
    | Focused | Unfocused -> false
  in
  let row_attrs =
    Rn_spec.Attrs.create ~attrs:(on_click :: focused_attr) ()
  in
  let cells =
    List.mapi Model.columns
      ~f:(fun i col ->
        let attrs =
          let focus_classes =
            match mode with
            | Focused | Editing ->
              if [%compare.equal:int option] (Some i) focused_column
              then ["cell-focused"]
              else []
            | Unfocused -> []
          in
          let highlighting_classes =
            let highlighting =
              if String.(=) (Column.name col) "position"
              then (Option.to_list style)
              else []
            in
            if [%compare.equal:int option] (Some i) sort_column
            then begin
              match mode with
              | Focused | Editing -> highlighting
              | Unfocused -> "sort-column" :: highlighting
            end
            else highlighting
          in
          let on_click = Attr.on_click (fun _ -> focus_nth_column i) in
          [ on_click; Attr.classes (focus_classes @ highlighting_classes) ]
        in
        { Rn_spec.Cell.
          attrs = Rn_spec.Attrs.create ~attrs ()
        ; node = column_cell ~editing ~remember_edit m col
        }
      )
  in
  { Rn_spec. row_attrs; cells }

let random_stock () : Model.t =
  let symbol =
    let rchar () = Char.to_int 'A' + Random.int 26 |> Char.of_int_exn in
    String.init 4 ~f:(fun (_:int) -> rchar ())
  in
  let fair = 10. +. Float.of_int (Random.int 10000) /. 100. in
  let bsize = (1 + Random.int 20) * 100 in
  let asize = Int.max 100 (bsize + 100 * (Random.int 5 - 2)) in
  let bid = fair -. Float.of_int (Random.int 20) /. 100. in
  let ask = fair +. Float.of_int (Random.int 20) /. 100. in
  let edge = Float.of_int (Random.int 10) /. 100. in
  let max_edge = edge +. Float.of_int (Random.int 10) /. 100. in
  let position = (Random.int 500 - 250) * 100 in
  let last_fill = None in
  let trader =
    let names = ["hsimmons"; "bkent"; "qhayes"; "gfernandez"] in
    List.nth_exn names (Random.int (List.length names))
  in
  { symbol; edge; max_edge; trader; bsize; asize; bid; ask; position; last_fill }

let random_rows n =
  List.init n ~f:(fun _ -> random_stock ())
