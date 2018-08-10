open Virtual_dom
open Core_kernel

type ('action, 'model, 'state, 'extra) with_extra =
  { apply_action : 'action -> 'state -> schedule_action:('action -> unit) -> 'model
  ; update_visibility : unit -> 'model
  ; view : Vdom.Node.t
  ; on_display : 'state -> schedule_action:('action -> unit) -> unit
  ; extra : 'extra
  }
[@@deriving fields]

type ('action, 'state, 'model) t = ('action, 'state, 'model, unit) with_extra

let create_with_extra ?apply_action ?update_visibility ?on_display ~extra model view =
  let apply_action =
    Option.value apply_action ~default:(fun _ _ ~schedule_action:_ -> model)
  in
  let update_visibility = Option.value update_visibility ~default:(fun _ -> model) in
  let on_display = Option.value on_display ~default:(fun _ ~schedule_action:_ -> ()) in
  { apply_action; update_visibility; on_display; extra; view }
;;

let create = create_with_extra ~extra:()
