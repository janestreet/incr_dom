open Js_of_ocaml

class type performance = object
  method mark : Js.js_string Js.t -> unit Js.meth

  method measure :
    Js.js_string Js.t
    -> Js.js_string Js.t
    -> Js.js_string Js.t
    -> PerformanceObserver.performanceEntry Js.t Js.meth

  method clearMarks : Js.js_string Js.t Js.optdef -> unit Js.meth
  method clearMeasures : Js.js_string Js.t Js.optdef -> unit Js.meth
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
  measure ~name ~start:before_name ~end_:after_name
  |> (ignore : PerformanceObserver.performanceEntry Js.t -> unit);
  res
;;

let clear_marks ?name () =
  (perf ())##clearMarks (Js.Optdef.map (Js.Optdef.option name) Js.string)
;;

let clear_measures ?name () =
  (perf ())##clearMeasures (Js.Optdef.map (Js.Optdef.option name) Js.string)
;;

module Manual = struct
  let mark = mark
  let measure = measure
end
