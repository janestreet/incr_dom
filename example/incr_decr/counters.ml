open! Core_kernel
open Incr_dom

(** The [Model] represents the full state of the application.  The module has methods for
    updating the model as well, which will be used when applying actions, in the
    [apply_action] function below.  *)
module Model = struct
  type t = { counters : int Int.Map.t } [@@deriving sexp, fields, equal]

  let add_new t = { counters = Map.set t.counters ~key:(Map.length t.counters) ~data:0 }

  (** Note that this function is total, because we want to handle the case where, e.g., a
      user tries to increment a counter that doesn't exist. (This could happen if we were
      to add a button to delete counters, and right after someone clicked it, before a
      refresh happened, they clicked another button to update the counter's field.) So,
      it's generally better to have a reasonable behavior for any action you receive. *)
  let update t ~pos ~diff =
    match Map.find t.counters pos with
    | None -> t
    | Some old_val -> { counters = Map.set t.counters ~key:pos ~data:(old_val + diff) }
  ;;

  (** It's important to specify a cutoff function that returns false if the models differ
      in any way.  Otherwise, you can get weird behavior where the application doesn't
      update in the expected way when the model changes.  Basing this on a derived
      equality or comparison function is a good way to do this.

      Note that the need for a cutoff function means that you need to avoid models that
      can't be checked for equality.  So, for example, including functions in your model
      is problematic. *)
  let cutoff t1 t2 = equal t1 t2
end

(** The [Action] type represents transactions whose purpose is to update the model, and
    maybe kick off other imperative actions.

    Note that the design of the action type is important!  In particular, you could
    imagine having [update] set an absolute value for the counter, rather than a diff.
    This, however, would work quite differently if you quickly clicked the button a few
    times in a row, before a refresh could occur.  Always be aware when designing the
    action type that actions will be interpreted at some unpredictable time after they're
    initiated. *)
module Action = struct
  type t =
    | New_counter
    | Update of
        { pos : int
        ; diff : int
        }
  [@@deriving sexp]

  let should_log _ = true
end

(** The state is for holding real imperative state, like RPC connections.  We have none of
    those here, so we make it trivial.  *)
module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | New_counter -> Model.add_new model
  | Update { pos; diff } -> Model.update model ~pos ~diff
;;

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let on_add_new_click = Attr.on_click (fun _ev -> inject New_counter) in
  let add_new_counter_button =
    Node.div [] [ Node.button [ on_add_new_click ] [ Node.text "add new counter" ] ]
  in
  let button txt ~pos ~diff =
    let on_click _ev = inject (Update { pos; diff }) in
    Node.button [ Attr.on_click on_click ] [ Node.text txt ]
  in
  let elements =
    Map.mapi m.counters ~f:(fun ~key:pos ~data:value ->
      let button_minus = button "-" ~pos ~diff:(-1) in
      let button_plus = button "+" ~pos ~diff:1 in
      Node.div [] [ button_minus; Node.text (Int.to_string value); button_plus ])
  in
  Node.body [] (add_new_counter_button :: Node.hr [] :: Map.data elements)
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  (* Here we use Incremental in a trivial way, just having everything recompute every time
     the model changes.  That approach is actually just fine for most small applications.
     Only use Incremental where you need to! *)
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view
;;
