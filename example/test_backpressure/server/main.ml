open Core
open Async

type t =
  { vanilla_rpc_server : Tcp.Server.inet
  ; web_server : Simple_web_server.t
  }

let subscribe () () =
  let read, write = Pipe.create () in
  (don't_wait_for
   @@
   let rec write_to_pipe i =
     if i % 1000 = 0 then print_s [%message (i : int)];
     let%bind () = Pipe.write write i in
     write_to_pipe (i + 1)
   in
   write_to_pipe 0);
  Deferred.return (Ok read)
;;

let implementations =
  [ Rpc.Pipe_rpc.implement Backpressure_test_shared.Protocol.Fibonacci.t subscribe ]
;;

module Mode = struct
  type t = Authenticated_via_krb of Krb.Mode.Server.t
end

let create_and_serve
      ~vanilla_rpc_port
      ~(serve_mode : Mode.t)
      ~(http_settings : Http_settings.t)
      ~js_path
  =
  let module Connection_index = Unique_id.Int () in
  let implementations =
    Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
  in
  let http_handler =
    let open Cohttp_static_handler in
    let handler = Single_page_handler.default_with_body_div ~div_id:"app" in
    Single_page_handler.create_handler
      handler
      ~assets:
        [ Asset.local Asset.Kind.javascript (Asset.What_to_serve.file ~path:js_path) ]
      ~on_unknown_url:`Not_found
  in
  let initial_connection_state get_principal identity initiated_from addr conn =
    let i = Connection_index.create () in
    let principal = get_principal identity in
    let message =
      [%message
        "New rpc connection"
          (initiated_from : Rpc_websocket_jane.Rpc.Connection_initiated_from.t)
          (principal : Krb.Principal.Name.t)
          (addr : Socket.Address.Inet.t)
          (i : Connection_index.t)]
    in
    Log.Global.sexp ~level:`Debug message;
    don't_wait_for
      (let%map reason = Rpc.Connection.close_reason ~on_close:`started conn in
       let message =
         [%message
           "Closed rpc connection"
             (reason : Info.t)
             (principal : Krb.Principal.Name.t)
             (addr : Socket.Address.Inet.t)
             (i : Connection_index.t)]
       in
       Log.Global.sexp ~level:`Debug message);
    ()
  in
  let on_handler_error inet err =
    Log.Global.sexp
      ~level:`Error
      [%message "Error encountered" (inet : Socket.Address.Inet.t) (err : exn)]
  in
  let rpc_config get_principal =
    Simple_web_server.Rpc_config.create
      ~implementations
      ~initial_connection_state:(initial_connection_state get_principal)
  in
  let vanilla_rpc_server, web_server =
    match serve_mode with
    | Authenticated_via_krb krb_mode ->
      let vanilla_rpc_server =
        Krb.Rpc.Connection.serve
          ~authorize:Krb.Authorize.accept_all
          ~implementations
          ~initial_connection_state:(fun identity inet conn ->
            initial_connection_state
              Krb.Client_identity.client_principal
              identity
              Rpc_websocket_jane.Rpc.Connection_initiated_from.Tcp
              inet
              conn)
          ~where_to_listen:(Tcp.Where_to_listen.of_port vanilla_rpc_port)
          ~krb_mode
          ()
      in
      let web_server =
        Simple_web_server.create
          ~authorize:Krb_http.Authorize.accept_all
          ~rpc_config:(rpc_config Fn.id)
          ~on_handler_error:(`Call on_handler_error)
          http_settings
          (const http_handler)
      in
      vanilla_rpc_server, web_server
  in
  let open Deferred.Or_error.Let_syntax in
  let%map vanilla_rpc_server = vanilla_rpc_server
  and web_server = web_server in
  { vanilla_rpc_server; web_server }
;;

let close_finished { vanilla_rpc_server; web_server } =
  let%map () = Tcp.Server.close_finished vanilla_rpc_server
  and () = Simple_web_server.close_finished web_server in
  Ok ()
;;

let command =
  Command.async_or_error
    ~summary:"RPC server for the Incr_decr app"
    (let%map_open.Command vanilla_rpc_port =
       flag
         "vanilla-rpc-port"
         (optional_with_default 8082 int)
         ~doc:"PORT for serving vanilla Async-RPC"
     and http_settings = Http_settings.param ()
     and js_path = anon ("FILE path to JavaScript file" %: string)
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind krb_mode = Deferred.ok (Krb.Mode.Server.kerberized ()) in
       let%bind server =
         create_and_serve
           ~serve_mode:(Authenticated_via_krb krb_mode)
           ~vanilla_rpc_port
           ~http_settings
           ~js_path
       in
       close_finished server)
    ~behave_nicely_in_pipeline:false
;;


let () =
  Command.group ~summary:"Commands for the Incr_decr RPC example" [ "server", command ]
  |> Command_unix.run
;;
