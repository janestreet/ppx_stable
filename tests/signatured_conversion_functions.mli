(** A test to maintain ppx_stable generated type signatures between variants and 
    polymorphic variants *)
open Base

module Stable_variant : sig
  module V1 : sig
    type t =
      | A of int * string
      | B of string
  end

  module V2 : sig
    type t =
      | A of bool * string
      | B of string

    val to_V1_t : modify_A:(bool -> string -> V1.t) -> t -> V1.t
    val of_V1_t : modify_A:(int -> string -> t) -> V1.t -> t
    val to_V1 : t -> V1.t
    val of_V1 : V1.t -> t
  end
end

module Stable_polymorphic_variant : sig
  module V1 : sig
    type t =
      [ `A of int * string
      | `B of string
      ]
  end

  module V2 : sig
    type t =
      [ `A of bool * string
      | `B of string
      ]

    val to_V1_t : modify_A:(bool -> string -> V1.t) -> t -> V1.t
    val of_V1_t : modify_A:(int -> string -> t) -> V1.t -> t
    val to_V1 : t -> V1.t
    val of_V1 : V1.t -> t
  end
end
