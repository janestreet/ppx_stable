open Base
open Ppxlib

(** {2 Attribute and extension names} *)

val ppx : which_ppx:[ `Record | `Variant ] -> string
val stable_record : string
val stable_variant : string

(** Used for polymorphic variants *)
val stable : string

(** Used to get change information from delta-types *)
val stable_changes : string

(** {3 PPX arguments} *)

val add : string
val modify : string
val set : string
val remove : string
val version : string

(** {2 OCaml constructs this ppx defines} *)

val stable_variant_module : type_name:string -> string
val helper_module : string
val recurse : string
val map : string
val removed : string
val modified : string

(** {3 Conversion function} *)

val remove_field : string -> string
val modify_field : string -> string

val conversion_function
  :  dir:[< `Of | `To ]
  -> source:string
  -> target:core_type
  -> string

(** {2 Things to operate on} *)

val variants : string
val fields : string
