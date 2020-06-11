module Model : sig
  type t

  val cutoff : t -> t -> bool
  val empty : t
end

include Incr_dom.App_intf.S with module Model := Model
