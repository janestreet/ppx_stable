open Ppx_helpers.Base_lite
open Ppxlib

val all_disjoints
  :  loc:location
  -> add:String.Set.t
  -> remove:String.Set.t
  -> modify:String.Set.t
  -> set:String.Set.t
  -> unit

val things_are_known
  :  loc:location
  -> all:String.Set.t
  -> thing_name:string
  -> supposed_to_be:string
  -> String.Set.t
  -> unit
