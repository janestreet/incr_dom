open! Core_kernel
module Incr = Incremental.Make ()

(* This is the default clock for running clients, but bonsai tests have their
   own separate clocks *)
let clock = Incr.Clock.create ~start:(Time_ns.now ()) ()

include Incr
module Map = Incr_map.Make (Incr)
module Select = Incr_select.Make (Incr)
