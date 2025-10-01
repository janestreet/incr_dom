open! Core
open! Async_kernel
open! Import

type ('model, 'action, 'state) t =
  { model_var : 'model Incr.Var.t
  ; old_model_var : 'model Incr.Var.t
  ; state : 'state
  ; view_incr : Vdom.Node.t Incr.t
  ; app_obs : ('action, 'model, 'state) Incr_dom.Component.t Incr.Observer.t
  ; action_queue : 'action Queue.t
  ; set_time : Time_ns.t -> unit
  ; sexp_of_model : 'model -> Sexp.t
  }
[@@deriving fields ~getters]

let create
  (type model action state)
  ~(initial_model : model)
  ~sexp_of_model
  ~initial_state:state
  (module App : Incr_dom.App_intf.S
    with type Model.t = model
     and type Action.t = action
     and type State.t = state)
  =
  let action_queue = Queue.create () in
  let module Event =
    Vdom.Effect.Define (struct
      module Action = App.Action

      let handle value ~on_exn:_ = Queue.enqueue action_queue value
    end)
  in
  let inject = Event.inject in
  let model_var = Incr.Var.create initial_model in
  let model_incr = Incr.Var.watch model_var in
  let old_model_var = Incr.Var.create initial_model in
  let old_model_incr = Incr.Var.watch old_model_var in
  Incr.set_cutoff
    model_incr
    (Incr.Cutoff.create (fun ~old_value ~new_value ->
       App.Model.cutoff old_value new_value));
  let app_incr = App.create model_incr ~old_model:old_model_incr ~inject in
  let app_obs = Incr.observe app_incr in
  let set_time to_ = Incr.Clock.advance_clock Incr.clock ~to_ in
  let view_incr = Incr.map app_incr ~f:Incr_dom.Component.view in
  { model_var
  ; old_model_var
  ; state
  ; view_incr
  ; app_obs
  ; action_queue
  ; set_time
  ; sexp_of_model
  }
;;

let perform_update t =
  let schedule_action = Queue.enqueue t.action_queue in
  let apply_action action =
    let apply_action =
      t.app_obs |> Incr.Observer.value_exn |> Incr_dom.Component.apply_action
    in
    let new_model = apply_action action t.state ~schedule_action in
    Incr.Var.set t.model_var new_model;
    Incr.stabilize ()
  in
  Incr.stabilize ();
  while not (Queue.is_empty t.action_queue) do
    apply_action (Queue.dequeue_exn t.action_queue)
  done;
  Incr.Var.set t.old_model_var (Incr.Var.value t.model_var)
;;
