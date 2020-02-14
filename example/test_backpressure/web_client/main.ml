open! Core_kernel
open Async_js_jane
open Async_kernel

let () = Async_js_jane.init ()

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let () =
  Async_kernel.don't_wait_for
  @@ let%bind conn =
       Rpc.Connection.client_exn
         ()
         ~check_hg_version_upon_connection:(Yes { on_version_mismatch = Alert })
  in
  let%bind pipe, _ =
    Rpc.Pipe_rpc.dispatch_exn Backpressure_test_shared.Protocol.Fibonacci.t conn ()
  in
  Pipe.iter pipe ~f:(fun i ->
    let fib_i = fib i in
    print_s [%message (i : int) (fib_i : int)];
    Deferred.unit)
;;
