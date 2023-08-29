(** Error logic for abstract types and code-gen for polymorphic variants.
    Creates [to_] and [of_] functions, as well as [Stable_variant.Helper...] *)

open Base
open Ppxlib

val create_ast_structure_items
  :  loc:location
  -> add:(label, String.comparator_witness) Set.t
  -> modify:(label, String.comparator_witness) Set.t
  -> remove:(label, String.comparator_witness) Set.t
  -> set:(label, String.comparator_witness) Set.t
  -> target_type:core_type option
  -> current_type:core_type
  -> rec_flag:rec_flag
  -> manifest:core_type option
  -> type_name:string
  -> ppx_name:string
  -> structure_item list
