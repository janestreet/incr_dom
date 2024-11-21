open Core

(* We generate this function once per datatype because otherwise the comparisons won’t
   get inlined, leading to pretty expensive float-array handling in the [float] case. *)
(*$
  open Core

  let func name modname =
    printf
      {|
        let %s arr value =
          let rec loop arr i last_index value =
            if %s.(<=) value arr.(i)
            then i
            else if i = last_index
            (* In this case, we hit the infinity bucket (for which there is no [bucket_ends]
               entry) *)
            then i    +   1
            else loop arr (i + 1) last_index value
          in
          loop arr 0 (Array.length arr - 1) value
        ;;
         |}
      name
      modname
  ;;

  List.iter
    [ "float", "Float"; "int", "Int"; "span", "Time_ns.Span" ]
    ~f:(fun (fname, mname) -> func fname mname)
*)
let float arr value =
  let rec loop arr i last_index value =
    if Float.( <= ) value arr.(i)
    then i
    else if i = last_index
            (* In this case, we hit the infinity bucket (for which there is no [bucket_ends]
               entry) *)
    then i + 1
    else loop arr (i + 1) last_index value
  in
  loop arr 0 (Array.length arr - 1) value
;;

let int arr value =
  let rec loop arr i last_index value =
    if Int.( <= ) value arr.(i)
    then i
    else if i = last_index
            (* In this case, we hit the infinity bucket (for which there is no [bucket_ends]
               entry) *)
    then i + 1
    else loop arr (i + 1) last_index value
  in
  loop arr 0 (Array.length arr - 1) value
;;

let span arr value =
  let rec loop arr i last_index value =
    if Time_ns.Span.( <= ) value arr.(i)
    then i
    else if i = last_index
            (* In this case, we hit the infinity bucket (for which there is no [bucket_ends]
               entry) *)
    then i + 1
    else loop arr (i + 1) last_index value
  in
  loop arr 0 (Array.length arr - 1) value
;;
(*$*)
