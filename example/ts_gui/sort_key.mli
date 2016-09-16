open! Core_kernel.Std
open! Import

type t =
  | String of string
  | Float of float
[@@deriving sexp]

include Comparable with type t := t
