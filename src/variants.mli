(** Code-gen for both variants and polymorphic variants. Creates [to_] and [of_]
    functions, as well as [Stable_variant.Helper...] *)

open Ppx_helpers.Base_lite
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
  -> add:String.Set.t
  -> modify:String.Set.t
  -> remove:String.Set.t
  -> set:String.Set.t
  -> target_type:core_type option
  -> current_type:core_type
  -> rec_flag:rec_flag
  -> variant_info:Info.t
  -> stackify:bool
  -> structure_item list
