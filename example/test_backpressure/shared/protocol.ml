open Core_kernel
open Async_rpc_kernel

module Fibonacci = struct
  let t =
    Rpc.Pipe_rpc.create
      ~name:"subscribe"
      ~version:0
      ~bin_query:bin_unit
      ~bin_response:bin_int
      ~bin_error:bin_unit
      ()
  ;;
end
