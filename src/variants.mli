(** Code-gen for both variants and polymorphic variants.
    Creates [to_] and [of_] functions, as well as [Stable_variant.Helper...] *)

open Base
open Ppxlib

module Info : sig
  type t

  val of_cdl : type_name:string -> constructor_declaration list -> t

  val of_row_fields
    :  loc:location
    -> ppx_name:string
    -> type_name:string
    -> row_field list
    -> t
end

val create_ast_structure_items
  :  loc:location
  -> add:(string, String.comparator_witness) Set.t
  -> modify:(string, String.comparator_witness) Set.t
  -> remove:(string, String.comparator_witness) Set.t
  -> set:(string, String.comparator_witness) Set.t
  -> target_type:core_type option
  -> current_type:core_type
  -> rec_flag:rec_flag
  -> variant_info:Info.t
  -> structure_item list
