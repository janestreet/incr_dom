open! Core_kernel

module Model : sig
  type t [@@deriving sexp]

  include Incr_dom.App_intf.Model with type t := t
end

module Action : sig
  type t =
    | New_counter
    | Update of
        { pos : int
        ; diff : int
        }
  [@@deriving sexp_of]
end

val initial_model_exn : (int * int) list -> Model.t

include
  Incr_dom.App_intf.S
  with module Model := Model
   and module Action := Action
   and type State.t = unit
