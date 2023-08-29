open Ppxlib
open Ast_builder.Default

let mk_lident ~loc str = Located.mk ~loc (Lident str)

let mk_module ~loc ~name ~items =
  pstr_module
    ~loc
    (module_binding
       ~loc
       ~name:(Located.mk ~loc (Some name))
       ~expr:(pmod_structure ~loc items))
;;

(* fun ~name:name -> exp *)
let mk_pexp_fun ~loc ~name exp = pexp_fun ~loc (Labelled name) None (pvar ~loc name) exp
