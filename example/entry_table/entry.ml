open! Core_kernel
open Poly
open! Import

module Model = struct
  module Focusable_field = struct
    module T = struct
      type t =
        | Fnorb
        | Snoot
        | Blimp
        | Snip
      [@@deriving sexp, enumerate, compare]
    end

    include T
    include Comparable.Make (T)

    let to_string t = sexp_of_t t |> Sexp.to_string

    let to_int = function
      | Fnorb -> 0
      | Snoot -> 1
      | Blimp -> 2
      | Snip -> 3
    ;;
  end

  (** The part of the model that changes rarely.  We've segmented this into a separate
      record to make it more efficient to recognize that nothing here has changed. *)
  module Basic = struct
    type t =
      { name : string
      ; fnorb : float Dirpair.t
      ; snoot : float Dirpair.t
      ; blimp : float Dirpair.t
      ; snip : float Dirpair.t
      ; collapsed : bool
      }
    [@@deriving sexp, fields, compare, equal]

    let get t ((ff : Focusable_field.t), dir) =
      let dp =
        match ff with
        | Fnorb -> t.fnorb
        | Snoot -> t.snoot
        | Blimp -> t.blimp
        | Snip -> t.snip
      in
      Dirpair.get dp dir
    ;;

    let fset t ((ff : Focusable_field.t), dir) v =
      let fset_dp dp = Dirpair.fset_dir dp dir v in
      match ff with
      | Fnorb -> { t with fnorb = fset_dp t.fnorb }
      | Snoot -> { t with snoot = fset_dp t.snoot }
      | Blimp -> { t with blimp = fset_dp t.blimp }
      | Snip -> { t with snip = fset_dp t.snip }
    ;;
  end

  type t =
    { basic : Basic.t
    ; live : float Dirpair.t
    }
  [@@deriving fields, sexp, compare, equal]

  let name t = t.basic.name

  let get_focus_point (field, dir) =
    let dir_int =
      match dir with
      | Buy -> 0
      | Sell -> 1
    in
    Focus_point.create [ Focusable_field.to_int field; dir_int ]
  ;;

  let full_focus_map =
    List.cartesian_product Focusable_field.all [ Buy; Sell ]
    |> List.map ~f:(fun x -> get_focus_point x, x)
    |> Focus_point.Map.of_alist_exn
  ;;

  let collapsed_fields = Focusable_field.Set.of_list [ Fnorb; Snoot ]

  let collapsed_focus_map =
    Map.to_alist full_focus_map
    |> List.filter ~f:(fun (_, (ff, _)) -> Set.mem collapsed_fields ff)
    |> Focus_point.Map.of_alist_exn
  ;;

  let focus_map ~collapsed = if collapsed then collapsed_focus_map else full_focus_map

  let move_focus t fp dir =
    let focus_map = focus_map ~collapsed:t.basic.collapsed in
    move_map_focus focus_map fp dir
  ;;

  let kick t =
    let kick = 0.01 *. float (Random.int 3 - 1) in
    let buy = t.live.buy +. kick in
    let sell = buy +. 0.01 in
    { t with live = { buy; sell } }
  ;;

  let example =
    let rng = Random.State.make_self_init () in
    fun () ->
      let name =
        String.init 4 ~f:(fun _ ->
          Char.to_int 'A' + Random.State.int rng 26 |> Char.of_int_exn)
      in
      let dp buy sell = { Dirpair.buy; sell } in
      { basic =
          { name
          ; fnorb = dp 0.1 0.1
          ; snoot = dp 33.12 33.13
          ; blimp = dp 1.02 1.02
          ; snip = dp 0.03 0.03
          ; collapsed = true
          }
      ; live = dp 33.12 33.13
      }
  ;;
end

module Action = struct
  type t =
    | Bump of bump_dir
    | Toggle_collapse
  [@@deriving sexp]

  let apply t focus_point (m : Model.t) =
    match t with
    | Bump bdir ->
      let amt =
        match bdir with
        | Incr -> 0.01
        | Decr -> -0.01
      in
      (match focus_point with
       | None -> m
       | Some focus_point ->
         (match Map.find (Model.focus_map ~collapsed:m.basic.collapsed) focus_point with
          | None -> m
          | Some pos ->
            let basic =
              Model.Basic.fset m.basic pos (Model.Basic.get m.basic pos +. amt)
            in
            { m with basic }))
    | Toggle_collapse ->
      let basic = { m.basic with collapsed = not m.basic.collapsed } in
      { m with basic }
  ;;
end

open Vdom

type focus_state =
  | Unfocused
  | Focused of Focus_point.t option

let field_is_focused ~collapsed (focus : focus_state) pos =
  let focus_map = Model.focus_map ~collapsed in
  match focus with
  | Unfocused -> false
  | Focused focus_point ->
    Some pos = Option.bind focus_point ~f:(fun x -> Map.find focus_map x)
;;

let align_left f attrs nodes = f (Attr.create "align" "left" :: attrs) nodes
let th = align_left Node.th
let td = align_left Node.td

let maybe_fields ~collapsed ff x =
  if collapsed && not (Set.mem Model.collapsed_fields ff) then [] else x
;;

let header ~collapsed ~set_inner_focus focus =
  let field_is_focused = field_is_focused ~collapsed focus in
  let dstring = function
    | Buy -> "/b"
    | Sell -> "/s"
  in
  let live_header =
    let s = "live" in
    [ th [] [ Node.text (s ^ dstring Buy) ]; th [] [ Node.text (s ^ dstring Sell) ] ]
  in
  let bd ff dir =
    th
      [ Attr.class_ (if field_is_focused (ff, dir) then "focused" else "unfocused")
      ; Attr.on_click (fun _ -> set_inner_focus (Model.get_focus_point (ff, dir)))
      ]
      [ Node.text (Model.Focusable_field.to_string ff ^ dstring dir) ]
  in
  let field_header ff = maybe_fields ~collapsed ff [ bd ff Buy; bd ff Sell ] in
  Node.tr
    []
    ([ th [] [ Node.text "name" ] ]
     @ field_header Fnorb
     @ field_header Snoot
     @ field_header Blimp
     @ field_header Snip
     @ live_header)
;;

(** Computes the header and the data corresponding to the slow changing part of the
    view.  Note that the code here doesn't depend on the live part of the model, which is
    important for performance reasons. *)
let basic_data_and_header ~set_inner_focus ~focus (basic : Model.Basic.t) =
  (* Note that the returned incremental only fully fires when the field in question
     changes. *)
  let collapsed = basic.collapsed in
  let field_is_focused = field_is_focused ~collapsed focus in
  let view_field ff =
    let num dir = Model.Basic.get basic (ff, dir) in
    maybe_fields
      ~collapsed
      ff
      (let attrs dir =
         [ Attr.class_ (if field_is_focused (ff, dir) then "focused" else "unfocused")
         ; Attr.on_click (fun _ -> set_inner_focus (Model.get_focus_point (ff, dir)))
         ]
       in
       [ td (attrs Buy) [ Node.text (Float.to_string_12 (num Buy)) ]
       ; td (attrs Sell) [ Node.text (Float.to_string_12 (num Sell)) ]
       ])
  in
  (* Map out the individual components of [t], so that we don't have to recompute the
     entire view every time live data ticks *)
  let name_node = [ td [] [ Node.text basic.name ] ] in
  let fnorb = view_field Fnorb in
  let snoot = view_field Snoot in
  let blimp = view_field Blimp in
  let snip = view_field Snip in
  let basic_data = name_node @ fnorb @ snoot @ blimp @ snip in
  let header = header ~collapsed ~set_inner_focus focus in
  basic_data, header
;;

(** Returns an incremental displaying the live ticking part of the arb display.
    Importantly, this incremental stops updating when the element in question is out of
    view, thus reducing the churn on the DOM and on the incremental graph. *)
let live_data visible live =
  let open Incr.Let_syntax in
  Incr.if_
    visible
    ~then_:
      (let%map live = live in
       [ td [] [ Node.text (Float.to_string_12 live.buy) ]
       ; td [] [ Node.text (Float.to_string_12 live.sell) ]
       ])
    ~else_:(Incr.const [ td [] [ Node.text "--" ]; td [] [ Node.text "--" ] ])
;;

let view
      (m : Model.t Incr.t)
      (entry_id : Entry_id.t)
      ~visible
      ~focus
      ~focus_me
      ~set_inner_focus
  =
  let open Incr.Let_syntax in
  let entry_id_attr = Attr.id (Entry_id.id_string entry_id) in
  let key_attr = Attr.string_property "key" (Entry_id.id_string entry_id) in
  let live = m >>| Model.live in
  let basic = m >>| Model.basic in
  let live_data = live_data visible live in
  let%bind focus = focus in
  let%map basic_data, header = basic >>| basic_data_and_header ~set_inner_focus ~focus
  and live_data = live_data in
  let data = Node.tr [] (basic_data @ live_data) in
  let table =
    let focused =
      match focus with
      | Focused _ -> true
      | Unfocused -> false
    in
    Node.table
      ((if focused then [ Attr.id "keep-in-view" ] else [])
       @ [ Attr.class_ (if focused then "focused" else "unfocused")
         ; Attr.on_click (fun _ -> focus_me)
         ])
      [ header; data ]
  in
  Node.div [ entry_id_attr; key_attr ] [ table ]
;;
