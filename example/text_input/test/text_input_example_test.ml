open! Core
open! Incr_dom_testing
module App = Incr_dom_text_input_example_lib.App

let make_helpers () =
  let driver =
    Driver.create
      ~initial_model:App.initial_model
      ~sexp_of_model:App.Model.sexp_of_t
      ~initial_state:()
      (module App)
  in
  Helpers.make driver
;;

let%expect_test "default model" =
  let (module H) = make_helpers () in
  H.show_view ();
  [%expect
    {|
    <body>
      <div> No submissions yet </div>
      <input id="input" type="text" #value="Default #0" oninput={handler}> </input>
      <button id="submit" onclick={handler}> Submit </button>
      <button id="reset" onclick={handler}> Reset </button>
      <button id="increment" onclick={handler}> Increment </button>
    </body> |}]
;;

let%expect_test "submit with default" =
  let (module H) = make_helpers () in
  H.click_on ~selector:"#submit";
  H.perform_update ();
  H.show_view ();
  [%expect
    {|
    <body>
      <div> Your latest submission was: Default #0 </div>
      <input id="input" type="text" #value="Default #0" oninput={handler}> </input>
      <button id="submit" onclick={handler}> Submit </button>
      <button id="reset" onclick={handler}> Reset </button>
      <button id="increment" onclick={handler}> Increment </button>
    </body> |}]
;;

let%expect_test "Increment twice and then submit" =
  let (module H) = make_helpers () in
  H.click_on ~selector:"#increment";
  H.click_on ~selector:"#increment";
  H.click_on ~selector:"#submit";
  H.perform_update ();
  H.show_view ();
  [%expect
    {|
    <body>
      <div> Your latest submission was: Default #2 </div>
      <input id="input" type="text" #value="Default #2" oninput={handler}> </input>
      <button id="submit" onclick={handler}> Submit </button>
      <button id="reset" onclick={handler}> Reset </button>
      <button id="increment" onclick={handler}> Increment </button>
    </body> |}]
;;

let%expect_test "Set value and then submit" =
  let (module H) = make_helpers () in
  H.input_text ~selector:"#input" ~text:"hello world";
  H.click_on ~selector:"#submit";
  H.perform_update ();
  H.show_view ();
  [%expect
    {|
    <body>
      <div> Your latest submission was: hello world </div>
      <input id="input" type="text" #value="hello world" oninput={handler}> </input>
      <button id="submit" onclick={handler}> Submit </button>
      <button id="reset" onclick={handler}> Reset </button>
      <button id="increment" onclick={handler}> Increment </button>
    </body> |}]
;;

let%expect_test "Set value and then increment" =
  let (module H) = make_helpers () in
  H.input_text ~selector:"#input" ~text:"hello world";
  H.click_on ~selector:"#increment";
  H.click_on ~selector:"#submit";
  H.perform_update ();
  H.show_view ();
  [%expect
    {|
    <body>
      <div> Your latest submission was: Default #1 </div>
      <input id="input" type="text" #value="Default #1" oninput={handler}> </input>
      <button id="submit" onclick={handler}> Submit </button>
      <button id="reset" onclick={handler}> Reset </button>
      <button id="increment" onclick={handler}> Increment </button>
    </body> |}]
;;
