open Js_of_ocaml

class type performance = object
  method mark : Js.js_string Js.t -> unit Js.meth

  method measure :
    Js.js_string Js.t -> Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method clearMarks : unit Js.meth
  method clearMeasures : unit Js.meth
end

let perf () : performance Js.t = Js.Unsafe.global##.performance
let mark name = (perf ())##mark (Js.string name)

let measure ~name ~start ~end_ =
  (perf ())##measure (Js.string name) (Js.string start) (Js.string end_)
;;

let record name ~f =
  let before_name = name ^ "_before" in
  let after_name = name ^ "_after" in
  let () = mark before_name in
  let res = f () in
  let () = mark after_name in
  measure ~name ~start:before_name ~end_:after_name;
  res
;;

let clear_marks () = (perf ())##clearMarks
let clear_measures () = (perf ())##clearMeasures

module Manual = struct
  let mark = mark
  let measure = measure
end
