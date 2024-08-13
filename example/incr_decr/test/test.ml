open! Core
open! Incr_dom_testing
module App = Incr_decr_example_lib.App

let make_helpers alist =
  let driver =
    Driver.create
      ~initial_model:(App.initial_model_exn alist)
      ~sexp_of_model:App.Model.sexp_of_t
      ~initial_state:()
      (module App)
  in
  Helpers.make driver
;;

let%expect_test "empty counters model" =
  let (module H) = make_helpers [] in
  H.show_view ();
  [%expect
    {|
    <body>
      <div>
        <button @on_click> add new counter </button>
      </div>
      <hr> </hr>
    </body>
    |}]
;;

let%expect_test "singleton initial model" =
  let (module H) = make_helpers [ 0, 13 ] in
  H.show_view ();
  [%expect
    {|
    <body>
      <div>
        <button @on_click> add new counter </button>
      </div>
      <hr> </hr>
      <div>
        <button @on_click> - </button>
        13
        <button @on_click> + </button>
      </div>
    </body>
    |}]
;;

let%expect_test "add some counters" =
  let (module H) = make_helpers [] in
  H.show_model ();
  [%expect {| ((counters ())) |}];
  H.do_actions [ App.Action.New_counter; App.Action.New_counter ];
  H.perform_update ();
  H.show_model ();
  [%expect {| ((counters ((0 0) (1 0)))) |}]
;;

let%expect_test "increment a counter" =
  let (module H) = make_helpers [ 0, 0 ] in
  H.show_model ();
  [%expect {| ((counters ((0 0)))) |}];
  H.do_actions [ App.Action.Update { pos = 0; diff = 5 } ];
  H.perform_update ();
  H.show_model ();
  [%expect {| ((counters ((0 5)))) |}]
;;
