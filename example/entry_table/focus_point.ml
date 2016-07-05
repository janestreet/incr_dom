open! Core_kernel.Std
open! Import

module T = struct
  type t = int list [@@deriving bin_io, sexp, compare]
  let create = Fn.id
  let hash = Hashtbl.hash
  let module_name = "Focus_point"
end

module T1 = struct
  include T
  include Sexpable.To_stringable(T)
end


include T1
include Identifiable.Make(T1)
