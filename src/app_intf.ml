open! Core
open Virtual_dom
open Async_kernel

(** Common module types *)
module type Model = sig
  type t


  (** A function for testing whether the model has changed enough to require refiring
      the incremental graph.

      It's best if the values in the model support a semantically reasonable cutoff
      function which lets you avoid infinite recomputation loops that can otherwise be
      triggered by the visibility checks. For this reason, it's typically a good idea to
      avoid having simple closures stored in the model.

      That said, it does work if you put phys_equal in for the cutoff. *)
  val cutoff : t -> t -> bool
end

module type Action = sig
  type t [@@deriving sexp_of]
end

module type State = sig
  (** Represents the imperative state associated with an application, typically used for
      housing things like communication Async-RPC connections. *)
  type t
end

module type S = sig
  module Model : Model
  module Action : Action
  module State : State

  (** [on_startup] is called once, right after the initial DOM is set to the view that
      corresponds to the initial state. This is useful for doing things like starting up
      async processes.  Note that this part of the computation does not support any
      incrementality, since it's only run once. *)
  val on_startup : schedule_action:(Action.t -> unit) -> Model.t -> State.t Deferred.t

  (** [create] is a function that incrementally constructs a {!Component}. Note that a
      [Component] supports functions like [apply_action], which return a new [Model.t],
      without taking a model as an explicit input.  The intent is for [apply_action] to
      have access to the current model via its construction

      Here's an example of how this might look in practice.

      {[
        module Model = struct
          type t = { counter : int } [@@deriving fields, compare]

          let cutoff t1 t2 = compare t1 t2 = 0
        end

        module State = struct
          type t = unit
        end

        module Action = struct
          type t = Increment [@@deriving sexp_of]

          let should_log _ = false
        end

        let initial_model = { Model.counter = 0 }

        let on_startup ~schedule_actions _model =
          every (Time_ns.Span.of_sec 1.) (fun () ->
            schedule_actions [ Action.Increment ]);
          Deferred.unit
        ;;

        let create model ~old_model:_ ~inject:_ =
          let open Incr.Let_syntax in
          let%map apply_action =
            let%map counter = model >>| Model.counter in
            fun (Increment : Action.t) _ ~schedule_actions:_ ->
              { Model.counter = counter + 1 }
          and view =
            let%map counter =
              let%map counter = model >>| Model.counter in
              Vdom.Node.div [] [ Vdom.Node.text (Int.to_string counter) ]
            in
            Vdom.Node.body [] [ counter ]
          and model = model in
          (* Note that we don't include [on_display] or [update_visibility], since
             these are optional arguments *)
          Component.create ~apply_action model view
        ;; ]}

      The full code for this example can be found in examples/counter.
  *)
  val create
    :  Model.t Incr.t
    -> old_model:Model.t Incr.t
    (** [old_model] contains the previous version of the model *)
    -> inject:(Action.t -> unit Vdom.Effect.t)
    (** [inject] gives you the ability to create event handlers in the virtual DOM. In
        your event handler, call this function on the action you would like to
        schedule. Virtual DOM will automatically delegate that action back to the
        [Start_app] main loop. *)
    -> (Action.t, Model.t, State.t) Component.t Incr.t
end
