open Base
open Ppxlib

let conversion_function ~dir ~source ~target =
  match target.ptyp_desc with
  | Ptyp_constr ({ txt = type_name; _ }, _) ->
    let fun_name =
      Printf.sprintf
        "%s_%s"
        (match dir with
         | `To -> "to"
         | `Of -> "of")
        (String.concat ~sep:"_" (Longident.flatten_exn type_name))
    in
    (match source with
     | "t" -> fun_name
     | _ -> source ^ "_" ^ fun_name)
  | _ -> assert false
;;

let modify_field name = "modify_" ^ name
let remove_field name = "remove_" ^ name
let stable_record = "stable_record"
let stable_variant = "stable_variant"

let ppx ~which_ppx =
  match which_ppx with
  | `Record -> stable_record
  | `Variant -> stable_variant
;;

let helper_module = "Helper"

let stable_variant_module ~type_name =
  match type_name with
  | "t" -> "Stable_variant"
  | _ -> "Stable_variant_of_" ^ type_name
;;

let recurse = "recurse"
let map = "map"
let fields = "fields"
let removed = "removed"
let modified = "modified"
let set = "set"
let stable = "stable"
let variants = "variants"
let add = "add"
let modify = "modify"
let remove = "remove"
let version = "version"
let stable_changes = "stable.changes"
