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

    let show_view ?selector () =
      let node = view_obs |> Incr.Observer.value_exn |> Node_helpers.unsafe_convert_exn in
      let node =
        match selector with
        | None -> node
        | Some selector -> Node_helpers.select_first_exn node ~selector
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
      let node = view_obs |> Incr.Observer.value_exn |> Node_helpers.unsafe_convert_exn in
      Node_helpers.select_first_exn node ~selector
    ;;

    let click_on ~selector =
      let element = get_element ~selector in
      Node_helpers.User_actions.click_on element
    ;;

    let input_text ~selector ~text =
      let element = get_element ~selector in
      Node_helpers.User_actions.input_text element ~text
    ;;
  end)
;;
