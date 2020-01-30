open! Core_kernel
open! Incr_dom_testing
module App = Incr_dom_counter_example_lib.App

let%expect_test _ =
  let driver =
    Driver.create
      ~initial_model:App.initial_model
      ~sexp_of_model:App.Model.sexp_of_t
      ~initial_state:()
      (module App)
  in
  let module H = (val Helpers.make driver) in
  H.show_view ();
  [%expect {|
    <body>
      <div> 0 </div>
    </body> |}];
  H.do_actions [ App.Action.Increment ];
  H.perform_update ();
  H.show_model ();
  [%expect {| ((counter 1)) |}];
  H.show_view ();
  [%expect {|
    <body>
      <div> 1 </div>
    </body> |}]
;;
