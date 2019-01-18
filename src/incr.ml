open! Core_kernel
module Incr = Incremental.Make ()

let clock = Incr.Clock.create ~start:(Time_ns.now ()) ()

include Incr
module Map = Incr_map.Make (Incr)
module Select = Incr_select.Make (Incr)
