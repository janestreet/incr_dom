open! Core_kernel.Std

let logf fmt =
  let open Js_of_ocaml in
  ksprintf (fun x -> Js.Unsafe.global##.console##log (Js.string x)) fmt
