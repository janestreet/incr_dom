module My_time = Time
open! Core_kernel
open! Import
module Time = My_time
module Rn_spec = Row_node_spec

module Model = struct
  type t =
    { symbol : string
    ; edge : float
    ; max_edge : float
    ; bsize : int
    ; bid : float
    ; ask : float
    ; asize : int
    ; position : int
    ; last_fill : Time.t option
    ; trader : string
    }
  [@@deriving compare, fields]

  let columns =
    let append f list field = f field :: list in
    let add ?group ?(editable=false) ?focus_on_edit ?sort_by m =
      append (fun field ->
        Column.of_field field m ~editable ?group ?sort_by ?focus_on_edit)
    in
    let num_f x = Sort_key.Float x in
    let num_i x ~f = Sort_key.Float (Float.of_int (f x)) in
    let num_i = num_i ~f:Fn.id in
    let time t =
      Sort_key.Float
        (Option.value ~default:Time_ns.epoch t
         |> Time_ns.to_int63_ns_since_epoch
         |> Int63.to_int64
         |> Float.of_int64)
    in
    let lex_s x = Sort_key.String x in
    let module Time_opt = struct
      type t = Time.t option

      let to_string = function
        | None -> "-"
        | Some x -> Time.to_string x
      ;;

      let of_string = function
        | "-" -> None
        | s -> Time.of_string s
      ;;
    end
    in
    let edge_group = "Edge data" in
    let market_group = "Market data" in
    let trade_group = "Trade data" in
    Fields.fold
      ~init:[]
      ~symbol:(add (module String) ~sort_by:lex_s)
      ~edge:
        (add
           (module Float)
           ~group:edge_group
           ~editable:true
           ~focus_on_edit:()
           ~sort_by:num_f)
      ~max_edge:(add (module Float) ~group:edge_group ~editable:true ~sort_by:num_f)
      ~trader:(add (module String) ~group:trade_group ~editable:true ~sort_by:lex_s)
      ~bsize:(add (module Int) ~group:market_group ~sort_by:num_i)
      ~bid:(add (module Float) ~group:market_group ~sort_by:num_f)
      ~ask:(add (module Float) ~group:market_group ~sort_by:num_f)
      ~asize:(add (module Int) ~group:market_group ~sort_by:num_i)
      ~position:(add (module Int) ~group:trade_group ~sort_by:num_i)
      ~last_fill:(add (module Time_opt) ~group:trade_group ~sort_by:time)
    |> List.rev
  ;;

  let matches_pattern t pattern =
    let matches s = String.is_substring ~substring:pattern (String.lowercase s) in
    matches t.symbol
  ;;

  let apply_edit t ~column value =
    match List.find columns ~f:(fun col -> Column.name col = column) with
    | None -> t
    | Some column ->
      (match Column.set column t value with
       | Error _ -> t
       | Ok t' -> t')
  ;;
end

module Action = struct
  type t =
    | Kick_price
    | Kick_position
  [@@deriving sexp]

  let kick_price = Kick_price

  let kick_position = Kick_position
end

(** Make sure that the number in question has at most two digits after the decimal point,
    so they render more cleanly.  *)
let fix_digits x = Float.round (x *. 100.) /. 100.
;;

let kick_price (m : Model.t) =
  let move = Float.of_int (Random.int 15 - 7) /. 100. in
  let spread = m.ask -. m.bid in
  let bid = fix_digits @@ Float.max 0.20 (m.bid +. move) in
  let ask = fix_digits @@ (bid +. spread) in
  { m with bid; ask }
;;

let kick_position (m : Model.t) =
  let position = m.position + (10 * (Random.int 21 - 10)) in
  { m with position; last_fill = Some (Time.now ()) }
;;

let apply_action (action : Action.t) (m : Model.t) =
  match action with
  | Kick_price -> kick_price m
  | Kick_position -> kick_position m
;;

module Mode = struct
  type t =
    | Unfocused
    | Focused
    | Editing
  [@@deriving sexp]
end

let editable_cell m col ~remember_edit =
  let open Vdom in
  let attrs =
    [ Attr.style [ "width", "100%" ]
    ; Attr.create "size" "1"
    ; Attr.value (Column.get col m)
    ; Attr.on_input (fun _ value -> remember_edit ~column:(Column.name col) value)
    ]
    @ if Column.focus_on_edit col then [ Attr.id "focus-on-edit" ] else []
  in
  Node.input attrs []
;;

let column_cell m col ~editing ~remember_edit =
  let open Vdom in
  if editing && Column.editable col
  then editable_cell m col ~remember_edit
  else Node.span [] [ Node.text (Column.get col m) ]
;;

let now = Incr.watch_now ()

module Rgb : sig
  type t =
    { r : int
    ; g : int
    ; b : int
    }

  val background_style : t -> (string * string) list

  (** [interpolate t1 t2 x] is equal to [t1] if [x=0], [t2] if [x=1], and interpolates
      between otherwise. *)
  val interpolate : t -> t -> float -> t
end = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    }
  [@@deriving fields]

  let attr_string { r; g; b } = sprintf "rgb(%d,%d,%d)" r g b

  let background_style t = [ "background-color", attr_string t ]

  let interpolate t_from t_to pct =
    let single field =
      let from = Field.get field t_from in
      let to_ = Field.get field t_to in
      from + Float.iround_nearest_exn (Float.of_int (to_ - from) *. pct)
    in
    Fields.map ~r:single ~g:single ~b:single
  ;;
end

let fade_out_color
      ~start_time
      ~solid_for
      ~fade_for
      ~(fade_from : Rgb.t)
      ~(fade_to : Rgb.t)
  =
  let%bind start_time = start_time in
  match start_time with
  | None -> return (Rgb.background_style fade_to)
  | Some start_time ->
    let end_solid = Time_ns.add start_time solid_for in
    let end_fade = Time_ns.add end_solid fade_for in
    let phase =
      Incr.step_function ~init:`Solid [ end_solid, `Fading; end_fade, `Default ]
    in
    (match%bind phase with
     | `Solid -> return (Rgb.background_style fade_from)
     | `Default -> return (Rgb.background_style fade_to)
     | `Fading ->
       let%map now = now in
       let elapsed = Time_ns.diff now end_solid in
       let pct_elapsed = Time_ns.Span.(elapsed // fade_for) in
       Rgb.background_style (Rgb.interpolate fade_from fade_to pct_elapsed))
;;

let highlight_color = { Rgb.r = 169; g = 101; b = 201 }

let normal_color = { Rgb.r = 245; g = 245; b = 245 }

let focused_color = { Rgb.r = 135; g = 206; b = 250 }

let sort_column_color = { Rgb.r = 250; g = 250; b = 200 }

let view
      (m : Model.t Incr.t)
      ~(mode : Mode.t Incr.t)
      ~sort_columns
      ~focused_column
      ~focus_me
      ~focus_nth_column
      ~remember_edit
  =
  let open Vdom in
  let on_click = Attr.on_click (fun _ -> focus_me) in
  let row_background_color (mode : Mode.t) ~is_sort_column =
    match mode with
    | Focused | Editing -> focused_color
    | Unfocused -> if is_sort_column then sort_column_color else normal_color
  in
  let last_fill_time = m >>| Model.last_fill in
  let%bind m = m and mode = mode and focused_column = focused_column in
  let editing =
    match mode with
    | Editing -> true
    | Focused | Unfocused -> false
  in
  let row_attrs = Rn_spec.Attrs.create () ~attrs:[ on_click ] in
  let%map last_fill_style =
    let sec x = Time_ns.Span.of_sec x in
    let is_sort_column =
      List.exists sort_columns ~f:(fun i ->
        match List.nth Model.columns i with
        | None -> false
        | Some c -> String.( = ) (Column.name c) "last_fill")
    in
    fade_out_color
      ~start_time:last_fill_time
      ~solid_for:(sec 1.)
      ~fade_for:(sec 0.5)
      ~fade_from:highlight_color
      ~fade_to:(row_background_color mode ~is_sort_column)
  in
  let cells =
    List.mapi Model.columns ~f:(fun i col ->
      let attrs =
        let focus_classes =
          match mode with
          | Focused ->
            if [%compare.equal: int option] (Some i) focused_column
            then [ "cell-focused" ]
            else []
          | Unfocused | Editing -> []
        in
        let on_click = Attr.on_click (fun _ -> focus_nth_column i) in
        [ on_click; Attr.classes focus_classes ]
      in
      let style =
        let is_sort_column = List.exists sort_columns ~f:(Int.equal i) in
        if String.( = ) (Column.name col) "last_fill"
        then last_fill_style
        else Rgb.background_style (row_background_color mode ~is_sort_column)
      in
      { Rn_spec.Cell.attrs = Rn_spec.Attrs.create ~attrs ~style ()
      ; node = column_cell ~editing ~remember_edit m col
      })
  in
  { Rn_spec.row_attrs; cells }
;;

let random_stock () : Model.t =
  let symbol =
    let rchar () = Char.to_int 'A' + Random.int 26 |> Char.of_int_exn in
    String.init 4 ~f:(fun (_ : int) -> rchar ())
  in
  let fair = 10. +. (Float.of_int (Random.int 10000) /. 100.) in
  let bsize = (1 + Random.int 20) * 100 in
  let asize = Int.max 100 (bsize + (100 * (Random.int 5 - 2))) in
  let bid = fix_digits (fair -. (Float.of_int (Random.int 20) /. 100.)) in
  let ask = fix_digits (fair +. (Float.of_int (Random.int 20) /. 100.)) in
  let edge = fix_digits (Float.of_int (Random.int 10) /. 100.) in
  let max_edge = fix_digits (edge +. (Float.of_int (Random.int 10) /. 100.)) in
  let position = (Random.int 500 - 250) * 100 in
  let last_fill = None in
  let trader =
    let names = [ "hsimmons"; "bkent"; "qhayes"; "gfernandez" ] in
    List.nth_exn names (Random.int (List.length names))
  in
  { symbol; edge; max_edge; trader; bsize; asize; bid; ask; position; last_fill }
;;

let random_rows n = List.init n ~f:(fun _ -> random_stock ())
