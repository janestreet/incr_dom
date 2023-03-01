open! Core
open! Async

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let main () =
  Deferred.ignore_m
    (let%bind a =
       Krb.Rpc.Connection.with_client
         ~authorize:Krb.Authorize.accept_all
         ~krb_mode:(Krb.Mode.Client.kerberized ())
         (Tcp.Where_to_connect.of_host_and_port
            (Host_and_port.of_string "localhost:12346"))
         (fun conn ->
            let%bind pipe, _ =
              Rpc.Pipe_rpc.dispatch_exn
                Backpressure_test_shared.Protocol.Fibonacci.t
                conn
                ()
            in
            Pipe.iter pipe ~f:(fun i ->
              let fib_i = fib i in
              print_s [%message (i : int) (fib_i : int)];
              Deferred.unit))
     in
     match a with
     | Ok () -> return ()
     | Error e -> Error.raise e)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ ( "run"
         , Command.async
             ~summary:""
             (let%map.Command () = Command.Spec.return () in
              fun () ->
                print_endline "hi";
                main ())
             ~behave_nicely_in_pipeline:false
         )
       ])
;;
