open! Core_kernel.Std
open! Import

module T = struct
  type t =
    | String of string
    | Float of float
  [@@deriving sexp, compare]
end
include T
include Comparable.Make(T)

