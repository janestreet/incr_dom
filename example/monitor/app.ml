open Core
open Async_kernel
open Incr_dom
open Js_of_ocaml
open Vdom

module Exn_location = struct
  type t =
    | None
    | Visibility
    | Action
    | Stabilization1
    | Stabilization2
    | Startup
    | On_display
    | On_keydown (* Exception in event handler does NOT cause the incr_dom app to stop *)
  [@@deriving sexp, compare, enumerate]

  let to_string t = sexp_of_t t |> Sexp.to_string_hum

  let of_string s =
    match Sexp.of_string s |> t_of_sexp with
    | t -> Some t
    | exception _ -> None
  ;;
end

module Model = struct
  type mode =
    | M1
    | M2
  [@@deriving compare]

  type t =
    { mode : mode
    ; exn_location : Exn_location.t
    ; monitor : Monitor.t
    ; stop : unit Ivar.t
    (* An Ivar in the model? really? *)
    }
  [@@deriving fields]

  let cutoff = phys_equal
end

module Action = struct
  type t =
    | Switch
    | Set_exn_location of Exn_location.t
    | Stop
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let fail_if_equal loc1 loc2 =
  if Exn_location.compare loc1 loc2 = 0
  then failwithf !"Failed on %{sexp:Exn_location.t}" loc1 ()
;;

let maybe_fail (m : Model.t) loc = fail_if_equal m.exn_location loc

let apply_action (m : Model.t) (action : Action.t) _ ~schedule_action:_ =
  maybe_fail m Action;
  match action with
  | Set_exn_location exn_location -> { m with exn_location }
  | Switch ->
    (match m.mode with
     | M1 -> { m with mode = M2 }
     | M2 -> { m with mode = M1 })
  | Stop ->
    Ivar.fill_if_empty m.stop ();
    m
;;

let update_visibility m ~schedule_action:_ =
  maybe_fail m Visibility;
  m
;;

let key_handler (m : Model.t) ~(inject : Action.t -> unit Vdom.Effect.t) =
  Attr.on_keydown (fun ev ->
    Async_kernel_scheduler.within_v ~monitor:m.monitor (fun () ->
      maybe_fail m On_keydown;
      match Dom_html.Keyboard_code.of_event ev with
      | KeyS -> inject Switch
      | KeyX -> inject Stop
      | _ -> Vdom.Effect.Ignore)
    |> Option.value ~default:Vdom.Effect.Ignore)
;;

let view (m : Model.t Incr.t) ~(inject : Action.t -> unit Vdom.Effect.t) =
  let open Incr.Let_syntax in
  let set_location =
    let open Js_of_ocaml in
    Vdom.Attr.on_change (fun (_ : Dom_html.event Js.t) value ->
      match Exn_location.of_string value with
      | Some loc -> inject (Set_exn_location loc)
      | None -> Vdom.Effect.Ignore)
  in
  let%map m = m in
  let attr = [ key_handler m ~inject ] in
  let text =
    match m.mode with
    | M1 ->
      maybe_fail m Stabilization1;
      Node.text "Model 1"
    | M2 ->
      maybe_fail m Stabilization2;
      Node.text "Model 2"
  in
  Node.body
    ~attrs:[ Attr.many_without_merge attr ]
    [ Node.div ~attrs:[ Attr.on_click (fun _ -> inject Switch) ] [ text ]
    ; Node.select
        ~attrs:[ set_location ]
        (List.map Exn_location.all ~f:(fun loc ->
           let s = Exn_location.to_string loc in
           let selected =
             if [%compare.equal: Exn_location.t] m.exn_location loc
             then [ Attr.create "selected" "selected" ]
             else []
           in
           Node.option
             ~attrs:[ Attr.many_without_merge (selected @ [ Attr.value s ]) ]
             [ Node.text s ]))
    ]
;;

let on_startup ~schedule_action:_ model =
  maybe_fail model Startup;
  Deferred.return ()
;;

let on_display (m : Model.t) _ ~schedule_action:_ =
  maybe_fail m On_display;
  ()
;;

let init ?init_loc monitor ~stop : Model.t =
  let exn_location =
    match init_loc with
    | None -> Exn_location.None
    | Some loc ->
      (match Exn_location.of_string loc with
       | Some x -> x
       | None -> Exn_location.Startup)
  in
  { mode = M1; exn_location; monitor; stop }
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and update_visibility =
    let%map model = model in
    update_visibility model
  and on_display =
    let%map model = model in
    on_display model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action ~update_visibility ~on_display model view
;;
