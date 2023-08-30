(* Analogous to [Basic_variant2] in [test.ml]. *)
module Basic_polymorphic_variant2 = struct
  type a
  type b
  type c
  type d
  type e
  type f
  type g
  type h
  type i
  type j
  type k
  type l

  module V1 = struct
    type t =
      [ `I0
      | `I1 of a
      | `I2 of b * c
      | `X1
      | `X2 of j
      | `X3 of k * l
      | `Z1 of d * e
      | `Z2 of f
      | `Z3
      ]
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
            ~i0:i0_fun
            ~i1:i1_fun
            ~i2:i2_fun
            ~x1:x1_fun
            ~x2:x2_fun
            ~x3:x3_fun
            ~z1:z1_fun
            ~z2:z2_fun
            ~z3:z3_fun
            = function
            | `I0 -> i0_fun ()
            | `I1 v0 -> i1_fun v0
            | `I2 (v0, v1) -> i2_fun v0 v1
            | `X1 -> x1_fun ()
            | `X2 v0 -> x2_fun v0
            | `X3 (v0, v1) -> x3_fun v0 v1
            | `Z1 (v0, v1) -> z1_fun v0 v1
            | `Z2 v0 -> z2_fun v0
            | `Z3 -> z3_fun ()
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      [ `I0
      | `I1 of a
      | `I2 of b * c
      | `Y1
      | `Y2 of g
      | `Y3 of h * i
      | `Z1
      | `Z2 of f
      | `Z3 of d * e
      ]
    [@@deriving_inline
      stable_variant
        ~version:V1.t
        ~remove:[ Y1; Y2; Y3 ]
        ~add:[ X1; X2; X3 ]
        ~modify:[ Z1; Z2; Z3 ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
            ~i0:i0_fun
            ~i1:i1_fun
            ~i2:i2_fun
            ~y1:y1_fun
            ~y2:y2_fun
            ~y3:y3_fun
            ~z1:z1_fun
            ~z2:z2_fun
            ~z3:z3_fun
            = function
            | `I0 -> i0_fun ()
            | `I1 v0 -> i1_fun v0
            | `I2 (v0, v1) -> i2_fun v0 v1
            | `Y1 -> y1_fun ()
            | `Y2 v0 -> y2_fun v0
            | `Y3 (v0, v1) -> y3_fun v0 v1
            | `Z1 -> z1_fun ()
            | `Z2 v0 -> z2_fun v0
            | `Z3 (v0, v1) -> z3_fun v0 v1
          ;;

          let _ = map
        end
      end

      let to_V1_t
        ~modify_Z1
        ~modify_Z2
        ~modify_Z3
        ~remove_Y3
        ~remove_Y2
        ~remove_Y1
        (v : t)
        : V1.t
        =
        Stable_variant.Helper.map
          v
          ~i0:(fun () -> `I0)
          ~i1:(fun v0 -> `I1 v0)
          ~i2:(fun v0 v1 -> `I2 (v0, v1))
          ~y1:remove_Y1
          ~y2:remove_Y2
          ~y3:remove_Y3
          ~z1:modify_Z1
          ~z2:modify_Z2
          ~z3:modify_Z3
      ;;

      let _ = to_V1_t

      let of_V1_t
        ~modify_Z1
        ~modify_Z2
        ~modify_Z3
        ~remove_X3
        ~remove_X2
        ~remove_X1
        (v : V1.t)
        : t
        =
        V1.Stable_variant.Helper.map
          v
          ~i0:(fun () -> `I0)
          ~i1:(fun v0 -> `I1 v0)
          ~i2:(fun v0 v1 -> `I2 (v0, v1))
          ~x1:remove_X1
          ~x2:remove_X2
          ~x3:remove_X3
          ~z1:modify_Z1
          ~z2:modify_Z2
          ~z3:modify_Z3
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [Basic_variant] in [parametric.ml]. *)
module Basic_parametric_polymorphic_variant = struct
  module V1 = struct
    type ('a, 'b, 'c, 'd, 'e, 'f, 'j, 'k, 'l) t =
      [ `I0
      | `I1 of 'a
      | `I2 of 'b * 'c
      | `X1
      | `X2 of 'j
      | `X3 of 'k * 'l
      | `Z1 of 'd * 'e
      | `Z2 of 'f
      | `Z3
      ]
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : ('a, 'b, 'c, 'd, 'e, 'f, 'j, 'k, 'l) t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
            ~i0:i0_fun
            ~i1:i1_fun
            ~i2:i2_fun
            ~x1:x1_fun
            ~x2:x2_fun
            ~x3:x3_fun
            ~z1:z1_fun
            ~z2:z2_fun
            ~z3:z3_fun
            = function
            | `I0 -> i0_fun ()
            | `I1 v0 -> i1_fun v0
            | `I2 (v0, v1) -> i2_fun v0 v1
            | `X1 -> x1_fun ()
            | `X2 v0 -> x2_fun v0
            | `X3 (v0, v1) -> x3_fun v0 v1
            | `Z1 (v0, v1) -> z1_fun v0 v1
            | `Z2 v0 -> z2_fun v0
            | `Z3 -> z3_fun ()
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t =
      [ `I0
      | `I1 of 'a
      | `I2 of 'b * 'c
      | `Y1
      | `Y2 of 'g
      | `Y3 of 'h * 'i
      | `Z1
      | `Z2 of 'f
      | `Z3 of 'd * 'e
      ]
    [@@deriving_inline
      stable_variant
        ~version:[%stable: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) V1.t]
        ~remove:[ Y1; Y2; Y3 ]
        ~add:[ X1; X2; X3 ]
        ~modify:[ Z1; Z2; Z3 ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
            ~i0:i0_fun
            ~i1:i1_fun
            ~i2:i2_fun
            ~y1:y1_fun
            ~y2:y2_fun
            ~y3:y3_fun
            ~z1:z1_fun
            ~z2:z2_fun
            ~z3:z3_fun
            = function
            | `I0 -> i0_fun ()
            | `I1 v0 -> i1_fun v0
            | `I2 (v0, v1) -> i2_fun v0 v1
            | `Y1 -> y1_fun ()
            | `Y2 v0 -> y2_fun v0
            | `Y3 (v0, v1) -> y3_fun v0 v1
            | `Z1 -> z1_fun ()
            | `Z2 v0 -> z2_fun v0
            | `Z3 (v0, v1) -> z3_fun v0 v1
          ;;

          let _ = map
        end
      end

      let to_V1_t
        ~modify_Z1
        ~modify_Z2
        ~modify_Z3
        ~remove_Y3
        ~remove_Y2
        ~remove_Y1
        (v : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t)
        : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) V1.t
        =
        Stable_variant.Helper.map
          v
          ~i0:(fun () -> `I0)
          ~i1:(fun v0 -> `I1 v0)
          ~i2:(fun v0 v1 -> `I2 (v0, v1))
          ~y1:remove_Y1
          ~y2:remove_Y2
          ~y3:remove_Y3
          ~z1:modify_Z1
          ~z2:modify_Z2
          ~z3:modify_Z3
      ;;

      let _ = to_V1_t

      let of_V1_t
        ~modify_Z1
        ~modify_Z2
        ~modify_Z3
        ~remove_X3
        ~remove_X2
        ~remove_X1
        (v : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) V1.t)
        : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t
        =
        V1.Stable_variant.Helper.map
          v
          ~i0:(fun () -> `I0)
          ~i1:(fun v0 -> `I1 v0)
          ~i2:(fun v0 v1 -> `I2 (v0, v1))
          ~x1:remove_X1
          ~x2:remove_X2
          ~x3:remove_X3
          ~z1:modify_Z1
          ~z2:modify_Z2
          ~z3:modify_Z3
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [Add_type_parameter_variant] in [parametric.ml] *)
module Add_type_parameter_polymorphic_variant = struct
  module V1 = struct
    type t = [ `Int of int ] [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~int:int_fun = function
            | `Int v0 -> int_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type 'a t =
      [ `Int of int
      | `Otherwise of 'a
      ]
    [@@deriving_inline stable_variant ~version:V1.t ~remove:[ Otherwise ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : 'a t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~int:int_fun ~otherwise:otherwise_fun = function
            | `Int v0 -> int_fun v0
            | `Otherwise v0 -> otherwise_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~remove_Otherwise (v : 'a t) : V1.t =
        Stable_variant.Helper.map v ~int:(fun v0 -> `Int v0) ~otherwise:remove_Otherwise
      ;;

      let _ = to_V1_t

      let of_V1_t (v : V1.t) : 'a t =
        V1.Stable_variant.Helper.map v ~int:(fun v0 -> `Int v0)
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [Change_type_parameter_variant] in [parametric.ml] *)
module Change_type_parameter_polymorphic_variant = struct
  module V1 = struct
    type 'a t = [ `Foo of 'a ] [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : 'a t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~foo:foo_fun = function
            | `Foo v0 -> foo_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type ('a, 'b) t =
      [ `Foo of 'a * int
      | `Bar of 'b
      ]
    [@@deriving_inline
      stable_variant ~version:[%stable: ('a * int) V1.t] ~modify:[ Foo ] ~remove:[ Bar ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : ('a, 'b) t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~foo:foo_fun ~bar:bar_fun = function
            | `Foo (v0, v1) -> foo_fun v0 v1
            | `Bar v0 -> bar_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~modify_Foo ~remove_Bar (v : ('a, 'b) t) : ('a * int) V1.t =
        Stable_variant.Helper.map v ~bar:remove_Bar ~foo:modify_Foo
      ;;

      let _ = to_V1_t

      let of_V1_t ~modify_Foo (v : ('a * int) V1.t) : ('a, 'b) t =
        V1.Stable_variant.Helper.map v ~foo:modify_Foo
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [Variant_basic] in [recursive.ml] *)
module Polymorphic_variant_basic = struct
  module V1 = struct
    type t =
      [ `A
      | `B of t
      | `C of t * t
      ]
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun ~c:c_fun = function
            | `A -> a_fun ()
            | `B v0 -> b_fun v0
            | `C (v0, v1) -> c_fun v0 v1
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      [ `A
      | `B of t
      | `C of t * t
      ]
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun ~c:c_fun = function
            | `A -> a_fun ()
            | `B v0 -> b_fun v0
            | `C (v0, v1) -> c_fun v0 v1
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~a:(fun () -> `A)
            ~b:(fun v0 -> `B (recurse v0))
            ~c:(fun v0 v1 -> `C (recurse v0, recurse v1))
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~a:(fun () -> `A)
            ~b:(fun v0 -> `B (recurse v0))
            ~c:(fun v0 v1 -> `C (recurse v0, recurse v1))
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [variant_nested] in [recursive.ml] *)
module Polymorphic_variant_nested = struct
  module V1 = struct
    type t =
      [ `A
      | `B of unit * (unit * t)
      ]
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun = function
            | `A -> a_fun ()
            | `B (v0, v1) -> b_fun v0 v1
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      [ `A
      | `B of unit * (unit * t)
      ]
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun = function
            | `A -> a_fun ()
            | `B (v0, v1) -> b_fun v0 v1
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~a:(fun () -> `A)
            ~b:(fun v0 v1 ->
              `B
                ( v0
                , let v0, v1 = v1 in
                  v0, recurse v1 ))
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~a:(fun () -> `A)
            ~b:(fun v0 v1 ->
              `B
                ( v0
                , let v0, v1 = v1 in
                  v0, recurse v1 ))
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [variant_persvasives] in [recursive.ml] *)
module Polymorphic_variant_pervasives = struct
  module V1 = struct
    type t =
      [ `Option of t option
      | `Ref of t ref
      | `Lazy_t of t lazy_t
      | `Array of t array
      | `List of t list
      ]
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
            ~option:option_fun
            ~ref:ref_fun
            ~lazy_t:lazy_t_fun
            ~array:array_fun
            ~list:list_fun
            = function
            | `Option v0 -> option_fun v0
            | `Ref v0 -> ref_fun v0
            | `Lazy_t v0 -> lazy_t_fun v0
            | `Array v0 -> array_fun v0
            | `List v0 -> list_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      [ `Option of t option
      | `Ref of t ref
      | `Lazy_t of t lazy_t
      | `Array of t array
      | `List of t list
      ]
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
            ~option:option_fun
            ~ref:ref_fun
            ~lazy_t:lazy_t_fun
            ~array:array_fun
            ~list:list_fun
            = function
            | `Option v0 -> option_fun v0
            | `Ref v0 -> ref_fun v0
            | `Lazy_t v0 -> lazy_t_fun v0
            | `Array v0 -> array_fun v0
            | `List v0 -> list_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~array:(fun v0 -> `Array (Stdlib.Array.map (fun x -> recurse x) v0))
            ~lazy_t:(fun v0 -> `Lazy_t (lazy (recurse (Stdlib.Lazy.force v0))))
            ~list:(fun v0 -> `List (Stdlib.List.map (fun x -> recurse x) v0))
            ~option:(fun v0 -> `Option (Stdlib.Option.map (fun x -> recurse x) v0))
            ~ref:(fun v0 -> `Ref (ref (recurse !v0)))
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~array:(fun v0 -> `Array (Stdlib.Array.map (fun x -> recurse x) v0))
            ~lazy_t:(fun v0 -> `Lazy_t (lazy (recurse (Stdlib.Lazy.force v0))))
            ~list:(fun v0 -> `List (Stdlib.List.map (fun x -> recurse x) v0))
            ~option:(fun v0 -> `Option (Stdlib.Option.map (fun x -> recurse x) v0))
            ~ref:(fun v0 -> `Ref (ref (recurse !v0)))
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

(* Analogous to [Variant_with_changes] in [recursive.ml] *)
module Polymorphic_variant_with_changes = struct
  module V1 = struct
    type t =
      [ `Mn of bool
      | `Mr of t
      | `Kn of bool
      | `Kr of t
      | `An of bool
      | `Ar of t
      ]
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~mn:mn_fun ~mr:mr_fun ~kn:kn_fun ~kr:kr_fun ~an:an_fun ~ar:ar_fun
            = function
            | `Mn v0 -> mn_fun v0
            | `Mr v0 -> mr_fun v0
            | `Kn v0 -> kn_fun v0
            | `Kr v0 -> kr_fun v0
            | `An v0 -> an_fun v0
            | `Ar v0 -> ar_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      [ `Mn of int
      | `Mr of t * t
      | `Kn of bool
      | `Kr of t
      | `Rn of bool
      | `Rr of t
      ]
    [@@deriving_inline
      stable_variant ~version:V1.t ~remove:[ Rn; Rr ] ~add:[ An; Ar ] ~modify:[ Mn; Mr ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~mn:mn_fun ~mr:mr_fun ~kn:kn_fun ~kr:kr_fun ~rn:rn_fun ~rr:rr_fun
            = function
            | `Mn v0 -> mn_fun v0
            | `Mr (v0, v1) -> mr_fun v0 v1
            | `Kn v0 -> kn_fun v0
            | `Kr v0 -> kr_fun v0
            | `Rn v0 -> rn_fun v0
            | `Rr v0 -> rr_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~modify_Mn ~modify_Mr ~remove_Rr ~remove_Rn =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~kn:(fun v0 -> `Kn v0)
            ~kr:(fun v0 -> `Kr (recurse v0))
            ~mn:modify_Mn
            ~mr:modify_Mr
            ~rn:remove_Rn
            ~rr:remove_Rr
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t ~modify_Mn ~modify_Mr ~remove_Ar ~remove_An =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~an:remove_An
            ~ar:remove_Ar
            ~kn:(fun v0 -> `Kn v0)
            ~kr:(fun v0 -> `Kr (recurse v0))
            ~mn:modify_Mn
            ~mr:modify_Mr
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end
