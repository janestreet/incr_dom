external js_prof_mark : string -> unit = "js_prof_mark"
external js_prof_measure : string -> string -> string -> unit = "js_prof_measure"

let mark name = js_prof_mark name
let measure ~name ~start ~end_ = js_prof_measure name start end_

let record name ~f =
  let before_name = name ^ "_before" in
  let after_name = name ^ "_after" in
  let () = mark before_name in
  let res = f () in
  let () = mark after_name in
  measure ~name ~start:before_name ~end_:after_name;
  res
;;

module Manual = struct
  let mark = mark
  let measure = measure
end
