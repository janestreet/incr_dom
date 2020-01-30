open! Core_kernel
open! Import
open Vdom_helpers
include Helpers_intf

let make (type model action state) driver
  : (module S with type model = model and type action = action and type state = state)
  =
  let view_obs = Incr.observe (Driver.view_incr driver) in
  Incr.stabilize ();
  (module struct
    type nonrec model = model
    type nonrec action = action
    type nonrec state = state

    let select_one_exn node ~selector =
      match Node_helpers.select_one node ~selector with
      | Some node -> node
      | None ->
        raise_s
          [%message
            "Failed to find element matching selector"
              (selector : string)
              ~from_node:(Node_helpers.to_string_html node : string)]
    ;;

    let show_view ?selector () =
      let node =
        view_obs |> Incr.Observer.value_exn |> Node_helpers.unsafe_convert_exn
      in
      let node =
        match selector with
        | None -> node
        | Some selector -> select_one_exn node ~selector
      in
      node |> Node_helpers.to_string_html |> print_endline
    ;;

    let show_model () =
      driver
      |> Driver.model_var
      |> Incr.Var.value
      |> Driver.sexp_of_model driver
      |> print_s
    ;;

    let do_actions actions = Queue.enqueue_all (Driver.action_queue driver) actions
    let set_model model = Incr.Var.set (Driver.model_var driver) model
    let perform_update () = Driver.perform_update driver

    let get_element ~selector =
      let node =
        view_obs |> Incr.Observer.value_exn |> Node_helpers.unsafe_convert_exn
      in
      select_one_exn node ~selector
    ;;

    let click_on ~selector =
      let element = get_element ~selector in
      Node_helpers.trigger element ~event_name:"onclick"
    ;;

    let input_text ~selector ~text =
      let open Js_of_ocaml in
      let element = get_element ~selector in
      let tag_name =
        match element with
        | Element { tag_name; _ } -> tag_name
        | other ->
          let node = Node_helpers.to_string_html other in
          raise_s [%message (node : string) "is not an element"]
      in
      let value_element =
        (* When an [on_input] event is fired, in order to pull the value of
           the element, [Virtual_dom.Vdom.Attr.on_input_event] looks at the
           "target" property on the event and tries to coerce that value to one
           of [input element, select element, textarea element].  This coercion
           function is implemented in [Js_of_ocaml.Dom_html.CoerceTo], and the
           way that the coercion function works is by comparing the value of
           the [tagName] property on the event target to the string of the tag
           name that the coercion is targeting.

           By mocking out the [tagName] and [value] properties on the target of
           the event, we can trick the virtual_dom code into handling our event
           as though there was a real DOM element! *)
        Js.Unsafe.inject
          (object%js
            val tagName = Js.string tag_name

            val value = Js.string text
          end)
      in
      Node_helpers.trigger
        element
        ~extra_fields:[ "target", value_element ]
        ~event_name:"oninput"
    ;;
  end)
;;
