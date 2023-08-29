open Base

module Stable_variant = struct
  module V1 = struct
    type t =
      | A of int * string
      | B of string
    [@@deriving stable_variant]
  end

  module V2 = struct
    type t =
      | A of bool * string
      | B of string
    [@@deriving stable_variant ~version:V1.t ~modify:[ A ]]

    let to_V1 = to_V1_t ~modify_A:(fun (_ : bool) s -> A (1, s))
    let of_V1 = of_V1_t ~modify_A:(fun (_ : int) s -> A (false, s))
  end
end

module Stable_polymorphic_variant = struct
  module V1 = struct
    type t =
      [ `A of int * string
      | `B of string
      ]
    [@@deriving stable_variant]
  end

  module V2 = struct
    type t =
      [ `A of bool * string
      | `B of string
      ]
    [@@deriving stable_variant ~version:V1.t ~modify:[ A ]]

    let to_V1 = to_V1_t ~modify_A:(fun (_ : bool) s -> `A (1, s))
    let of_V1 = of_V1_t ~modify_A:(fun (_ : int) s -> `A (false, s))
  end
end
