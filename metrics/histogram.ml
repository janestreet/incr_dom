open! Core

module Buckets = struct
  include Prometheus_instrumentation_collector_histogram_buckets.Buckets

  let bucket_ranges (type a) (buckets : a t) =
    let (zero : a), (inf : a) =
      match type_ buckets with
      | Int -> 0, Int.max_value
      | Float -> 0., Float.infinity
      | Span -> Time_ns.Span.zero, Time_ns.Span.max_value_representable
    in
    let boundaries = boundaries_excluding_infinity buckets |> Array.to_list in
    let bottoms = zero :: boundaries in
    let tops = boundaries @ [ inf ] in
    List.zip_exn bottoms tops
  ;;
end

module Find_bucket = Prometheus_instrumentation_collector_histogram_buckets.Find_bucket

type 'a t =
  { buckets : 'a Buckets.t
  ; counts : int array
  ; mutable sum : 'a
  ; mutable min : 'a option
  ; mutable max : 'a option
  }
[@@deriving equal]

module type S = sig
  type t [@@deriving compare, equal, sexp_of, quickcheck]

  val zero : t
  val ( + ) : t -> t -> t
  val scale_int_possibly_overflow_for_tests : t -> int -> t
end

let module_of_histogram (type a) (type_ : a Buckets.Type.t) =
  match type_ with
  | Float ->
    (module struct
      include Float

      let quickcheck_generator = Float.gen_incl (-10e100) 10e100
      let scale_int_possibly_overflow_for_tests t scale_by = t * of_int scale_by
    end : S
      with type t = a)
  | Int ->
    (module struct
      include Int

      let scale_int_possibly_overflow_for_tests = ( * )
    end)
  | Span ->
    (module struct
      include Time_ns.Span

      let scale_int_possibly_overflow_for_tests = scale_int
    end)
;;

let create (type a) ~(buckets : a Buckets.t) () =
  let bucket_at_infinity = 1 in
  let num_buckets =
    (Buckets.boundaries_excluding_infinity buckets |> Array.length) + bucket_at_infinity
  in
  let counts = Array.create ~len:num_buckets 0 in
  let (module M : S with type t = a) = module_of_histogram (Buckets.type_ buckets) in
  { buckets; counts; sum = M.zero; min = None; max = None }
;;

let bucket_idx (type a) (buckets : a Buckets.t) (value : a) =
  match Buckets.type_ buckets with
  | Float -> Find_bucket.float (Buckets.boundaries_excluding_infinity buckets) value
  | Int -> Find_bucket.int (Buckets.boundaries_excluding_infinity buckets) value
  | Span -> Find_bucket.span (Buckets.boundaries_excluding_infinity buckets) value
;;

let observe (type a) t datum =
  let bucket_idx = bucket_idx t.buckets datum in
  t.counts.(bucket_idx) <- t.counts.(bucket_idx) + 1;
  let (module M : S with type t = a) = module_of_histogram (Buckets.type_ t.buckets) in
  t.sum <- M.( + ) t.sum datum;
  t.min
  <- (match t.min with
      | None -> Some datum
      | Some min -> Some (Comparable.min M.compare min datum));
  t.max
  <- (match t.max with
      | None -> Some datum
      | Some max -> Some (Comparable.max M.compare max datum))
;;

let clone t =
  { buckets = t.buckets
  ; counts = Array.copy t.counts
  ; sum = t.sum
  ; min = t.min
  ; max = t.max
  }
;;

module For_sexp = struct
  type 'a outer_t = 'a t

  type 'a t =
    { boundaries : 'a array
    ; counts : int array
    ; sum : 'a
    ; min : 'a option
    ; max : 'a option
    }
  [@@deriving sexp_of]

  let t_of_outer_t (outer_t : 'a outer_t) =
    { boundaries = Buckets.boundaries_excluding_infinity outer_t.buckets
    ; counts = outer_t.counts
    ; sum = outer_t.sum
    ; min = outer_t.min
    ; max = outer_t.max
    }
  ;;
end

let sexp_of_t conv t = For_sexp.t_of_outer_t t |> For_sexp.sexp_of_t conv
