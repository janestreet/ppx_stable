open Base
open Ppxlib

val all_disjoints
  :  loc:location
  -> add:(string, 'cmp) Set.t
  -> remove:(string, 'cmp) Set.t
  -> modify:(string, 'cmp) Set.t
  -> set:(string, 'cmp) Set.t
  -> unit

val things_are_known
  :  loc:location
  -> all:(string, 'cmp) Set.t
  -> thing_name:string
  -> supposed_to_be:string
  -> (string, 'cmp) Set.t
  -> unit
