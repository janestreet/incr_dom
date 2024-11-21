open! Core
open Js_of_ocaml

module Dev_tools_color = struct
  type t =
    | Primary
    | Primary_light
    | Primary_dark
    | Secondary
    | Secondary_light
    | Secondary_dark
    | Tertiary
    | Tertiary_light
    | Tertiary_dark
    | Error
  [@@deriving sexp_of, string ~capitalize:"kebab-case"]
end

module Details = struct
  class type devtools = object
    method dataType : Js.js_string Js.t Js.Optdef.t Js.prop
    method track : Js.js_string Js.t Js.Optdef.t Js.prop
    method color : Js.js_string Js.t Js.Optdef.t Js.prop
  end

  class type t = object
    method devtools : devtools Js.t Js.readonly_prop
  end
end

module Measure_options = struct
  class type t = object
    method start : Js.number Js.t Js.optdef Js.prop
    method end_ : Js.number Js.t Js.optdef Js.prop
    method detail : Details.t Js.t Js.readonly_prop
  end

  let the_one_and_only : t Js.t =
    object%js
      val mutable start = Js.Optdef.empty
      val mutable end_ = Js.Optdef.empty

      val detail =
        object%js
          val devtools =
            object%js
              val mutable dataType = Js.Optdef.return (Js.string "track-entry")
              val mutable track = Js.Optdef.empty
              val mutable color = Js.Optdef.empty
            end
        end
    end
  ;;
end

module Mark_options = struct
  class type t = object
    method detail : Details.t Js.t Js.readonly_prop
  end

  let the_one_and_only : t Js.t =
    object%js
      val detail =
        object%js
          val devtools =
            object%js
              val mutable dataType = Js.Optdef.empty
              val mutable track = Js.Optdef.empty
              val mutable color = Js.Optdef.empty
            end
        end
    end
  ;;
end

class type performance = object
  method mark : Js.js_string Js.t -> Mark_options.t Js.t -> unit Js.meth
  method measure : Js.js_string Js.t -> Measure_options.t Js.t -> unit Js.meth
  method clearMarks : Js.js_string Js.t Js.optdef -> unit Js.meth
  method clearMeasures : Js.js_string Js.t Js.optdef -> unit Js.meth
  method now : Js.number Js.t Js.meth
end

let perf () : performance Js.t = Js.Unsafe.global##.performance

module Timer = struct
  type t =
    { start_ms : Js.number Js.t
    ; num_backgrounding_changes : int
    }

  type measurement =
    { start_ms : Js.number Js.t
    ; stop_ms : Js.number Js.t
    ; backgrounding_changed : bool
    }

  let start () =
    { start_ms = (perf ())##now
    ; num_backgrounding_changes = Bonsai_metrics.Private.num_backgrounding_changes ()
    }
  ;;

  let stop { start_ms; num_backgrounding_changes } =
    { start_ms
    ; stop_ms = (perf ())##now
    ; backgrounding_changed =
        not
          (Int.equal
             num_backgrounding_changes
             (Bonsai_metrics.Private.num_backgrounding_changes ()))
    }
  ;;

  let record f =
    let timer = start () in
    let r = f () in
    let t = stop timer in
    r, t
  ;;

  module Duration = struct
    type t =
      | Ok of Time_ns.Span.t
      | Backgrounding_changed_unreliable of Time_ns.Span.t
    [@@deriving sexp_of]
  end

  let duration { start_ms; stop_ms; backgrounding_changed } =
    let span =
      Time_ns.Span.of_ms (Js.float_of_number stop_ms -. Js.float_of_number start_ms)
    in
    if backgrounding_changed
    then Duration.Backgrounding_changed_unreliable span
    else Ok span
  ;;
end

let prominent_data_type = Js.Optdef.return (Js.string "marker")

(* Implementing this by overriding fields of a single global instance every time hopes to
   avoid allocating new objects. Mutating the object is safe, and also less effective,
   because Chrome clones the [details] field so it can later be read.

   This code is a bit fragile, because if we add a field, we might forget to set it.
   However, because we're setting every field every time, there's no risk of stale field
   values sticking around. *)
let mark ?(prominent = false) ?color name =
  Mark_options.the_one_and_only##.detail##.devtools##.color
  := Js.Optdef.option
       (Option.map color ~f:(fun x -> Js.string (Dev_tools_color.to_string x)));
  Mark_options.the_one_and_only##.detail##.devtools##.dataType
  := if prominent then prominent_data_type else Js.Optdef.empty;
  (perf ())##mark (Js.string name) Mark_options.the_one_and_only
;;

let measure ?color ?track name { Timer.start_ms; stop_ms; backgrounding_changed } =
  match backgrounding_changed with
  | true -> ()
  | false ->
    Measure_options.the_one_and_only##.start := Js.Optdef.return start_ms;
    Measure_options.the_one_and_only##.end_ := Js.Optdef.return stop_ms;
    Measure_options.the_one_and_only##.detail##.devtools##.track
    := Js.Optdef.option (Option.map track ~f:Js.string);
    Measure_options.the_one_and_only##.detail##.devtools##.color
    := Js.Optdef.option
         (Option.map color ~f:(fun x -> Js.string (Dev_tools_color.to_string x)));
    (perf ())##measure (Js.string name) Measure_options.the_one_and_only
;;

let clear_marks ?name () =
  (perf ())##clearMarks (Js.Optdef.map (Js.Optdef.option name) Js.string)
;;

let clear_measures ?name () =
  (perf ())##clearMeasures (Js.Optdef.map (Js.Optdef.option name) Js.string)
;;

let time_since_navigation_start () =
  Time_ns.Span.of_ms (Js.float_of_number (perf ())##now)
;;
