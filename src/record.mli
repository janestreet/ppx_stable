(** Code-gen for records. Creates [to_] and [of_] functions *)

open Base
open Ppxlib

val create_ast_structure_items
  :  loc:location
  -> fields:label_declaration list
  -> add:(label, String.comparator_witness) Set.t
  -> remove:(label, String.comparator_witness) Set.t
  -> modify:(label, String.comparator_witness) Set.t
  -> set:(label, String.comparator_witness) Set.t
  -> target_type:core_type option
  -> current_type:core_type
  -> rec_flag:rec_flag
  -> type_name:label
  -> structure_item list
