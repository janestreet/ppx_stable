(** Code-gen for records. Creates [to_] and [of_] functions *)

open Ppx_helpers.Base_lite
open Ppxlib

val create_ast_structure_items
  :  loc:location
  -> fields:label_declaration list
  -> add:String.Set.t
  -> remove:String.Set.t
  -> modify:String.Set.t
  -> set:String.Set.t
  -> target_type:core_type option
  -> current_type:core_type
  -> rec_flag:rec_flag
  -> type_name:label
  -> stackify:bool
  -> structure_item list
