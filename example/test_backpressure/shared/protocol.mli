open! Core
open Async_rpc_kernel

module Fibonacci : sig
  val t : (unit, int, unit) Rpc.Pipe_rpc.t
end
