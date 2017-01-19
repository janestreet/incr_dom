open Core_kernel.Std
open Js_of_ocaml

module T = struct
  include Time_ns
  let sexp_of_t = [%sexp_of: Time_ns.Alternate_sexp.t]
  let t_of_sexp = [%of_sexp: Time_ns.Alternate_sexp.t]
end

include T
include Comparable.Make(T)

let now () =
  let date = new%js Js.date_now in
  Time_ns.Span.of_ms date##getTime
  |> Time_ns.of_span_since_epoch

let to_string t =
  let date = new%js Js.date_fromTimeValue
    (Float.of_int64 (Int63.to_int64 (to_int63_ns_since_epoch t)) /. 1e6) in
  let str = date##toTimeString |> Js.to_string in
  fst (String.lsplit2_exn ~on:' ' str)

(* This function cannot be invoked, as we do not allow editing of times. However, since it
   is being displayed, and all columns support editing (even though we don't actually edit
   all of them) this function must be implemented.
*)
let of_string _ = assert false
