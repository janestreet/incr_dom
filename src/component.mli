(** A [Component] captures the basic operations that need to be provided in order to build
    an Incr_dom app.  The same type can be used both at the top-level of an app,
    as well as for defining individual components within a larger app.

    The {!Component.t} is often constructed incrementally, and is always created
    incrementally at the top-level, as is required by {!App_intf.S}.  *)

open Virtual_dom

(** The with_extra type allows the component to expose extra information that might be
    useful to the user of the component. This allows the module in which the component is
    defined to expose functions that can be used to query incrementally computed
    information about the the component. *)
type ('action, 'model, 'state, 'extra) with_extra

type ('action, 'model, 'state) t = ('action, 'model, 'state, unit) with_extra

(** [apply_action] is called to update the model in response to the arrival of a scheduled
    action. The function may have side effects, which among other things can trigger more
    actions. *)
val apply_action
  :  ('action, 'model, 'state, _) with_extra
  -> 'action
  -> 'state
  -> schedule_action:('action -> unit)
  -> 'model

(** [update_visibility] is called in order to allow the component to query the state of
    the DOM to determine what is currently in view, and put that information into the
    model. This in turn allows for the rendering to limit the set of DOM nodes actually
    created to those that are in sight.

    {!Start_app} calls [update_visibility] when a top-level resize or scroll is detected.
    Also, individual components that create scrollable widgets should create DOM event
    handlers that return the [Viewport_changed] event from {!Virtual_dom.Vdom.Event} when
    those widgets are scrolled or resized. Otherwise, visibility changes may not be
    correctly captured.

    Note that changes in visibility could trigger changes to the DOM that in turn cause
    the page to reflow. This can cause more changes in visibility, which can lead to an
    infinite loop.  Such behavior is a bug in the component, but the framework doesn't
    attempt to stop cascading sequences of [update_visibility], so that these bugs are not
    hidden. *)
val update_visibility
  :  ('action, 'model, _, _) with_extra
  -> schedule_action:('action -> unit)
  -> 'model

(** [view] is called to obtain the virtual DOM which is then applied to the actual DOM. *)
val view : _ with_extra -> Vdom.Node.t

(** [extra] allows you to expose extra information that was derived from the model. Note
    that for most components, [extra] has type [unit].  *)
val extra : (_, _, _, 'extra) with_extra -> 'extra

(** [on_display] is called every time the DOM is updated, with the model just before the
    update and the model just after the update. Use [on_display] to initiate actions. *)
val on_display
  :  ('action, _, 'state, _) with_extra
  -> 'state
  -> schedule_action:('action -> unit)
  -> unit

(** Though this create function is not incremental, it is usually called in the context of
    an incremental computation function, like the one in {!App_intf.S}. If some
    arguments are not supplied, defaults (which either return the model supplied or unit)
    are included in the component.

    Note that the functions [apply_action], [update_visibility] and [on_display] are
    allowed to perform effects, but the incremental computation that produces [create]
    should itself be functional.
*)
val create
  :  ?apply_action:('action -> 'state -> schedule_action:('action -> unit) -> 'model)
  -> ?update_visibility:(schedule_action:('action -> unit) -> 'model)
  -> ?on_display:('state -> schedule_action:('action -> unit) -> unit)
  -> 'model
  -> Vdom.Node.t
  -> ('action, 'model, 'state) t

(** Like {!create}, but allows for the specification of the [extra] parameter, which
    allows for component-specific state to be exposed. *)
val create_with_extra
  :  ?apply_action:('action -> 'state -> schedule_action:('action -> unit) -> 'model)
  -> ?update_visibility:(schedule_action:('action -> unit) -> 'model)
  -> ?on_display:('state -> schedule_action:('action -> unit) -> unit)
  -> extra:'extra
  -> 'model
  -> Vdom.Node.t
  -> ('action, 'model, 'state, 'extra) with_extra
