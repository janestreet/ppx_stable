(** Error logic for abstract types and code-gen for polymorphic variants. Creates [to_]
    and [of_] functions, as well as [Stable_variant.Helper...] *)

open Ppx_helpers.Base_lite
open Ppxlib

val create_ast_structure_items
  :  loc:location
  -> add:String.Set.t
  -> modify:String.Set.t
  -> remove:String.Set.t
  -> set:String.Set.t
  -> target_type:core_type option
  -> current_type:core_type
  -> rec_flag:rec_flag
  -> manifest:core_type option
  -> type_name:string
  -> ppx_name:string
  -> stackify:bool
  -> structure_item list
