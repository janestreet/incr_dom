open! Core
open Or_error.Let_syntax

module Type = struct
  type 'a t =
    | Float : float t
    | Int : int t
    | Span : Time_ns.Span.t t
  [@@deriving sexp_of]

  module Not_float = struct
    type 'a parent = 'a t

    (* We have this because [float] gets handled separately below because of the
       float-array hack and because of Infinity. *)
    type 'a t =
      | Int : int t
      | Time_ns_span : Time_ns.Span.t t
    [@@deriving sexp_of]

    (* This will only get called on k1 <> k2 if a user exposes that e.g.
       int = int63 = Time_ns.Span.t *)
    let compare _compare_a k1 k2 = Poly.compare k1 k2

    let widen (type a) (t : a t) : a parent =
      match t with
      | Int -> Int
      | Time_ns_span -> Span
    ;;

    let find (type a) (p : a parent) : (a t, (a, float) Type_equal.t) Either.t =
      match p with
      | Float -> Second T
      | Int -> First Int
      | Span -> First Time_ns_span
    ;;
  end
end

module Bucket_spec = struct
  type ('input_params, 'output_buckets) t =
    | Float : (float, float) t
    | Seconds : (float, Time_ns.Span.t) t
    | Span : (Time_ns.Span.t, Time_ns.Span.t) t
    | Int : (int, int) t

  let to_float : 'input. ('input, _) t -> 'input -> float =
    fun (type a x) (t : (a, x) t) (x : a) : float ->
    match t with
    | Float -> x
    | Seconds -> x
    | Span -> Time_ns.Span.to_sec x
    | Int -> Int.to_float x
  ;;

  let of_floats_exn : 'output. (_, 'output) t -> float array -> 'output array =
    fun (type a x) (t : (x, a) t) (floats : float array) : a array ->
    match t with
    | Float -> floats
    | Seconds -> Array.map floats ~f:Time_ns.Span.of_sec
    | Span -> Array.map floats ~f:Time_ns.Span.of_sec
    | Int ->
      let ints = Array.map floats ~f:Float.iround_nearest_exn in
      if (not (Array.is_sorted_strictly ints ~compare))
         && Array.is_sorted_strictly floats ~compare:Float.compare
      then
        Error.raise (Error.of_string "Bucket boundaries collided after rounding to ints");
      ints
  ;;

  let of_floats t floats = Or_error.try_with (fun () -> of_floats_exn t floats)

  let type_ (type a x) (t : (x, a) t) : a Type.t =
    match t with
    | Float -> Float
    | Seconds -> Span
    | Span -> Span
    | Int -> Int
  ;;
end

(* Floats are handled specially to avoid allocating floats from values in the array for
   comparisons, etc.

   The boundaries all implicitly include a final '+Inf' at the end.
*)
type 'a t =
  | Float : { boundaries_excluding_infinity : Array.Float.t } -> float t
  | Immediate :
      { boundaries : 'a Array.t
      ; type_ : 'a Type.Not_float.t
      }
      -> 'a t
[@@deriving sexp_of]

let comparisons (type a) (type_ : a Type.Not_float.t)
  : (module Comparisons.S with type t = a)
  =
  match type_ with
  | Int -> (module Int)
  | Time_ns_span -> (module Time_ns.Span)
;;

let compare (type a) compare_inner (t1 : a t) (t2 : a t) =
  match t1, t2 with
  | ( Float { boundaries_excluding_infinity = b1 }
    , Float { boundaries_excluding_infinity = b2 } ) -> Array.Float.compare b1 b2
  | Float _, Immediate _ -> -1
  | Immediate _, Float _ -> 1
  | Immediate { type_ = k1; boundaries = b1 }, Immediate { type_ = k2; boundaries = b2 }
    ->
    (match Type.Not_float.compare compare_inner k1 k2 with
     | 0 ->
       let (module M) = comparisons k1 in
       Array.compare M.compare b1 b2
     | x -> x)
;;

let equal _equal_inner =
  (* The [compare] function currently completely ignores [compare_inner] and forces
     the use of standard comparison functions for [int], [float] or [Time_ns.Span.t].
     To highlight this, we pass in [Nothing.unreachable_code]. If [compare] starts
     meaningfully using [compare_inner], this will stop compiling and we'll have to
     pass in whatever suits the new semantics of [compare_inner]. *)
  Comparable.equal (compare Nothing.unreachable_code)
;;

let drop_infinity (type a) (t : a t) : a t =
  match t with
  | Float { boundaries_excluding_infinity } ->
    if Array.mem boundaries_excluding_infinity Float.infinity ~equal:Float.equal
    then
      Float
        { boundaries_excluding_infinity =
            Array.filter boundaries_excluding_infinity ~f:(fun x -> Float.(x < infinity))
        }
    else t
  | Immediate _ ->
    (* We won't remove an entry at the max value from non-float buckets. This way, if the
       user would like to write down the max value instead of ‘Inf’ for the boundary, they
       can write it down explicitly. This doesn’t make much sense for int/span, but might
       for other fixed-point types if we add them. *)
    t
;;

let boundaries_excluding_infinity (type a) (t : a t) : a Array.t =
  match t with
  | Float { boundaries_excluding_infinity } -> boundaries_excluding_infinity
  | Immediate { type_ = _; boundaries } -> boundaries
;;

let type_ (type a) (t : a t) : a Type.t =
  match t with
  | Float _ -> Float
  | Immediate { type_; boundaries = _ } -> Type.Not_float.widen type_
;;

let validate : 'a. 'a t -> 'a t Or_error.t =
  fun (type a) (t_maybe_infinite : a t) : a t Or_error.t ->
  let not_nan =
    match t_maybe_infinite with
    | Immediate _ -> Ok ()
    | Float { boundaries_excluding_infinity } ->
      if Array.exists boundaries_excluding_infinity ~f:Float.is_nan
      then Or_error.error_string "Histogram buckets must not contain NaN."
      else Ok ()
  in
  let increasing =
    if match t_maybe_infinite with
       | Float { boundaries_excluding_infinity } ->
         Array.is_sorted_strictly ~compare:[%compare: float] boundaries_excluding_infinity
       | Immediate { type_; boundaries } ->
         let (module M) = comparisons type_ in
         Array.is_sorted_strictly ~compare:M.compare boundaries
    then Ok ()
    else Or_error.error_string "Histogram buckets must be strictly increasing"
  in
  let t = drop_infinity t_maybe_infinite in
  let non_empty =
    if match t with
       | Float { boundaries_excluding_infinity } ->
         Array.is_empty boundaries_excluding_infinity
       | Immediate { boundaries; type_ = _ } -> Array.is_empty boundaries
    then Or_error.error_string "Histogram buckets may not be empty"
    else Ok ()
  in
  let%map.Or_error () = Or_error.all_unit [ non_empty; not_nan; increasing ] in
  t
;;

let create_float ~boundaries =
  Float { boundaries_excluding_infinity = boundaries } |> validate
;;

let create_immediate ~boundaries ~type_ = Immediate { boundaries; type_ } |> validate

let create (type a) (type_ : a Type.t) ~(boundaries : a array) : a t Or_error.t =
  match Type.Not_float.find type_ with
  | First type_ -> create_immediate ~boundaries ~type_
  | Second T -> create_float ~boundaries
;;

module Buckets_helpers = struct
  let check_increasing_range ~range_start ~range_end =
    if Float.(range_start >= range_end)
    then Error (Error.of_string "Range start must be strictly smaller than end.")
    else Ok ()
  ;;

  let check_superunitary_count ~count =
    if count <= 1 then Error (Error.of_string "Count must be bigger than 1.") else Ok ()
  ;;

  let check_positive_step ~step =
    if Float.(step <= 0.) then Error (Error.of_string "Step must be positive") else Ok ()
  ;;

  let check_positive_range_start ~range_start =
    if Float.(range_start <= 0.)
    then Error (Error.of_string "Range needs a positive start value")
    else Ok ()
  ;;

  let check_superunitary_factor ~factor =
    if Float.(factor <= 1.)
    then Error (Error.of_string "The exponential factor must be greater than 1.")
    else Ok ()
  ;;
end

module Linear = struct
  let boundaries_from_step ~range_start ~step ~count =
    let%map () = Buckets_helpers.check_positive_step ~step
    and () = Buckets_helpers.check_superunitary_count ~count in
    Array.init count ~f:(fun index -> range_start +. (Float.of_int index *. step))
  ;;

  let create_from_step
    (type a b)
    (kind : (a, b) Bucket_spec.t)
    ~(range_start : a)
    ~(step : a)
    ~count
    =
    let to_float = Bucket_spec.to_float in
    let%bind floats =
      boundaries_from_step
        ~range_start:(to_float kind range_start)
        ~step:(to_float kind step)
        ~count
    in
    let%bind boundaries = Bucket_spec.of_floats kind floats in
    create (Bucket_spec.type_ kind) ~boundaries
  ;;

  let create_from_step_exn kind ~range_start ~step ~count =
    create_from_step kind ~range_start ~step ~count |> ok_exn
  ;;

  let create_from_range
    (type a b)
    (kind : (a, b) Bucket_spec.t)
    ~(range_start : a)
    ~(range_end : a)
    ~count
    =
    let to_float = Bucket_spec.to_float in
    let range_start = to_float kind range_start in
    let range_end = to_float kind range_end in
    let%bind () = Buckets_helpers.check_increasing_range ~range_start ~range_end in
    let step = (range_end -. range_start) /. Float.of_int (count - 1) in
    let%bind floats = boundaries_from_step ~range_start ~step ~count in
    let%bind boundaries = Bucket_spec.of_floats kind floats in
    create (Bucket_spec.type_ kind) ~boundaries
  ;;

  let create_from_range_exn kind ~range_start ~range_end ~count =
    create_from_range kind ~range_start ~range_end ~count |> ok_exn
  ;;
end

module Exponential = struct
  let boundaries_from_factor ~range_start ~factor ~count =
    let%map () = Buckets_helpers.check_superunitary_count ~count
    and () = Buckets_helpers.check_superunitary_factor ~factor
    and () = Buckets_helpers.check_positive_range_start ~range_start in
    Array.init count ~f:(fun index -> range_start *. (factor **. Float.of_int index))
  ;;

  let create_from_factor
    (type a b)
    (kind : (a, b) Bucket_spec.t)
    ~(range_start : a)
    ~(factor : float)
    ~count
    =
    let range_start = Bucket_spec.to_float kind range_start in
    let%bind floats = boundaries_from_factor ~range_start ~factor ~count in
    let%bind boundaries = Bucket_spec.of_floats kind floats in
    create (Bucket_spec.type_ kind) ~boundaries
  ;;

  let create_from_factor_exn kind ~range_start ~factor ~count =
    create_from_factor kind ~range_start ~factor ~count |> ok_exn
  ;;

  let create_from_range
    (type a b)
    (kind : (a, b) Bucket_spec.t)
    ~(range_start : a)
    ~(range_end : a)
    ~count
    =
    let to_float = Bucket_spec.to_float in
    let range_start = to_float kind range_start in
    let range_end = to_float kind range_end in
    let%bind () = Buckets_helpers.check_increasing_range ~range_start ~range_end in
    let factor = (range_end /. range_start) **. (1. /. Float.of_int (count - 1)) in
    let%bind floats = boundaries_from_factor ~range_start ~factor ~count in
    let%bind boundaries = Bucket_spec.of_floats kind floats in
    create (Bucket_spec.type_ kind) ~boundaries
  ;;

  let create_from_range_exn kind ~range_start ~range_end ~count =
    create_from_range kind ~range_start ~range_end ~count |> ok_exn
  ;;
end

module Expert = struct
  let create_of_array kind buckets = create kind ~boundaries:buckets
  let create kind buckets = create kind ~boundaries:(Nonempty_list.to_array buckets)
  let create_exn kind buckets = create kind buckets |> ok_exn

  let create_seconds_exn floats =
    create_exn Span (Nonempty_list.map ~f:Time_ns.Span.of_sec floats)
  ;;
end

module Predefined = struct
  let float =
    Expert.create_exn
      Float
      [ 0.0001; 0.001; 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5; 5.; 10. ]
  ;;

  let default =
    Expert.create_exn
      Span
      Time_ns.Span.
        [ of_int_us 100
        ; of_int_ms 1
        ; of_int_ms 5
        ; of_int_ms 10
        ; of_int_ms 25
        ; of_int_ms 50
        ; of_int_ms 100
        ; of_int_ms 250
        ; of_int_ms 500
        ; of_int_sec 1
        ; of_int_ms 2_500
        ; of_int_sec 5
        ; of_int_sec 10
        ]
  ;;
end
