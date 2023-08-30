open Ppxlib

let create_ast_structure_items
  ~loc
  ~add
  ~modify
  ~remove
  ~set
  ~target_type
  ~current_type
  ~rec_flag
  ~manifest
  ~type_name
  ~ppx_name
  =
  (* Polymorphic variants are abstract ptypes, but so are all of the non-record and
     non-variant types that we don't support. Both cases are handled here.

     Unlike records, we need to create the helper module for a variant regardless of
     whether a target type is specified. This is because future versions of such variant
     will need to use that module in their conversion functions. *)
  match manifest with
  | None -> Location.raise_errorf ~loc "%s: abstract types not supported" ppx_name
  | Some manifest ->
    (match manifest.ptyp_desc with
     | Ptyp_variant (row_fields, Closed, None) ->
       let variant_info =
         Variants.Info.of_row_fields ~ppx_name ~loc ~type_name row_fields
       in
       Variants.create_ast_structure_items
         ~loc
         ~add
         ~modify
         ~remove
         ~set
         ~target_type
         ~current_type
         ~rec_flag
         ~variant_info
     | Ptyp_any
     | Ptyp_var _
     | Ptyp_arrow _
     | Ptyp_tuple _
     | Ptyp_constr _
     | Ptyp_object _
     | Ptyp_class _
     | Ptyp_alias _
     | Ptyp_variant _
     | Ptyp_poly _
     | Ptyp_package _
     | Ptyp_extension _ ->
       (match target_type with
        | None -> []
        | Some _ ->
          Location.raise_errorf
            ~loc
            "%s: type %s not supported"
            ppx_name
            (Ppxlib.string_of_core_type manifest)))
;;
