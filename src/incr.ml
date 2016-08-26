open! Core_kernel.Std

module Incr = Incremental_kernel.Incremental.Make ()
include Incr
module Map = Incr_map.Make (Incr)
module Select = Incr_select.Make (Incr)

