open Base
open Ppxlib

val mk_lident : loc:location -> string -> longident loc
val mk_module : loc:location -> name:string -> items:structure -> structure_item
val mk_pexp_fun : loc:location -> name:string -> expression -> expression
