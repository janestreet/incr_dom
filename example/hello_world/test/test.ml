open! Core_kernel
open! Incr_dom_testing
module App = Hello_world_example_lib.App

let%expect_test "print contents" =
  let driver =
    Driver.create
      ~initial_model:App.initial_model
      ~sexp_of_model:[%sexp_of: unit]
      ~initial_state:()
      (module App)
  in
  let module H = (val Helpers.make driver) in
  H.show_view ();
  [%expect {| hello world |}]
;;
