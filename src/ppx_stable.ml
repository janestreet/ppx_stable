open Base
open Ppxlib
open Ast_builder.Default

module Changes_by_type = struct
  type 'a t =
    { add : 'a
    ; modify : 'a
    ; set : 'a
    ; remove : 'a
    }

  type kind =
    | Add
    | Modify
    | Set
    | Remove

  let set t kind value =
    match kind with
    | Add -> { t with add = value }
    | Modify -> { t with modify = value }
    | Set -> { t with set = value }
    | Remove -> { t with remove = value }
  ;;

  let get t kind =
    match kind with
    | Add -> t.add
    | Modify -> t.modify
    | Set -> t.set
    | Remove -> t.remove
  ;;

  let create x = { add = x; modify = x; set = x; remove = x }

  let map t ~f =
    { add = f t.add; modify = f t.modify; set = f t.set; remove = f t.remove }
  ;;

  let to_list t = [ t.add; t.modify; t.set; t.remove ]
end

let conversions_of_td ~ppx_name ~target_type ~rec_flag changes td =
  let ({ add; modify; set; remove } : _ Changes_by_type.t) = changes in
  let loc = td.ptype_loc in
  let add = Set.of_list (module String) add in
  let modify = Set.of_list (module String) modify in
  let remove = Set.of_list (module String) remove in
  let set = Set.of_list (module String) set in
  Invariants.all_disjoints ~loc ~add ~modify ~remove ~set;
  let current_type =
    Ast_helper.Typ.constr
      ~loc
      (Located.map_lident td.ptype_name)
      (List.map ~f:fst td.ptype_params)
  in
  let structures =
    match td.ptype_kind with
    | Ptype_open -> Location.raise_errorf ~loc "%s: open types not supported" ppx_name
    | Ptype_record lds ->
      (match target_type with
       | None -> Location.raise_errorf ~loc "%s: missing target version" ppx_name
       | Some _ ->
         Record.create_ast_structure_items
           ~loc
           ~fields:lds
           ~add
           ~remove
           ~modify
           ~set
           ~target_type
           ~current_type
           ~rec_flag
           ~type_name:td.ptype_name.txt)
    | Ptype_variant cdl ->
      let variant_info = Variants.Info.of_cdl cdl ~type_name:td.ptype_name.txt in
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
    | Ptype_abstract ->
      Abstract.create_ast_structure_items
        ~loc
        ~add
        ~modify
        ~remove
        ~set
        ~target_type
        ~current_type
        ~rec_flag
        ~manifest:td.ptype_manifest
        ~type_name:td.ptype_name.txt
        ~ppx_name
  in
  structures
;;

let fields_or_constructors () =
  let open Ast_pattern in
  let rec_fields_pat = elist (pexp_ident (lident __)) in
  let constrs_pat = elist (pexp_construct (lident __) none) in
  alt rec_fields_pat constrs_pat
;;

let type_pattern =
  let open Ast_pattern in
  let ident =
    map' (pexp_ident __) ~f:(fun loc _ lid ->
      Some (Ast_builder.Default.ptyp_constr ~loc (Located.mk ~loc lid) []))
  in
  let type_ =
    map' (* make sure we get a type constructor. *)
      (pexp_extension (extension (string Naming.stable) (ptyp (ptyp_constr __' __))))
      ~f:(fun loc _ lid params -> Some (Ast_builder.Default.ptyp_constr ~loc lid params))
  in
  alt ident type_
;;

let stable_changes =
  let raise_invalid_change_argument ~loc =
    Location.raise_errorf
      ~loc
      "Invalid change argument. Expected %s, %s, %s, or %s."
      Naming.add
      Naming.modify
      Naming.set
      Naming.remove
  in
  Attribute.declare
    Naming.stable_changes
    Type_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_apply (estring (string "")) __) nil ^:: nil))
    (fun args : _ Changes_by_type.t ->
      let init = Changes_by_type.create None in
      List.fold args ~init ~f:(fun acc (label, expression) ->
        let loc = expression.pexp_loc in
        let name =
          match label with
          | Labelled name -> name
          | Nolabel | Optional _ -> raise_invalid_change_argument ~loc
        in
        let kind : Changes_by_type.kind =
          match name with
          | "add" -> Add
          | "modify" -> Modify
          | "set" -> Set
          | "remove" -> Remove
          | _ -> raise_invalid_change_argument ~loc
        in
        let value = Ast_pattern.parse (fields_or_constructors ()) loc expression Fn.id in
        match Changes_by_type.get acc kind with
        | None -> Changes_by_type.set acc kind (Some value)
        | Some _ -> Location.raise_errorf ~loc "%s argument was passed twice" name)
      |> Changes_by_type.map ~f:(Option.value ~default:[]))
;;

let make_stable_changes_attribute
  ~loc
  ?(add = [])
  ?(modify = [])
  ?(set = [])
  ?(remove = [])
  ()
  =
  let open (val Ast_builder.make loc) in
  let mkident x =
    if Char.is_lowercase x.[0]
    then pexp_ident (Located.lident x)
    else pexp_construct (Located.lident x) None
  in
  let ident_list names = elist (List.map ~f:mkident names) in
  let change_expression =
    pexp_apply
      [%expr ""]
      [ Labelled Naming.add, ident_list add
      ; Labelled Naming.set, ident_list set
      ; Labelled Naming.modify, ident_list modify
      ; Labelled Naming.remove, ident_list remove
      ]
  in
  attribute
    ~name:(Located.mk (Attribute.name stable_changes))
    ~payload:(PStr [ pstr_eval change_expression [] ])
;;

let args =
  Deriving.Args.(
    let changes = pack2 (pexp_loc __ (fields_or_constructors ())) in
    empty
    +> arg Naming.version type_pattern
    +> arg Naming.add changes
    +> arg Naming.modify changes
    +> arg Naming.set changes
    +> arg Naming.remove changes)
;;

(* That's actually useless, it's just here so ppxlib's driver doesn't complain *)
let rewrite_type_ext =
  Extension.declare
    Naming.stable
    Extension.Context.expression
    Ast_pattern.(ptyp (ptyp_constr __' __))
    (fun ~loc ~path:_ _ _ ->
      [%expr `Do_not_use_percent_stable_outside_of_deriving_stable])
;;

let () = Driver.register_transformation Naming.stable ~extensions:[ rewrite_type_ext ]

let gen ppx_name ~loc ~path:_ (rec_flag, tds) target_type add modify set remove =
  match tds with
  | [ td ] ->
    let changes_from_args : _ Changes_by_type.t = { add; modify; set; remove } in
    let changes =
      match Attribute.get stable_changes td with
      | Some changes_from_attribute ->
        (match Changes_by_type.to_list changes_from_args |> List.find_map ~f:Fn.id with
         | None -> ()
         | Some (loc, _) ->
           Location.raise_errorf
             ~loc
             "The changes (%s, %s, %s, or %s) passed to\n\
              [@@@@deriving %s] are unnecessary. They are already\n\
              specified by the [@@@@stable.changes] attribute."
             Naming.add
             Naming.modify
             Naming.set
             Naming.remove
             ppx_name);
        changes_from_attribute
      | None ->
        Changes_by_type.map changes_from_args ~f:(Option.value_map ~f:snd ~default:[])
    in
    conversions_of_td ~ppx_name ~rec_flag ~target_type changes td
  | _ ->
    Location.raise_errorf
      ~loc
      "mutually recursive types are not supported by ppx_stable_type"
;;

let stable_record =
  let name = Naming.stable_record in
  let str_type_decl = Deriving.Generator.make args (gen name) in
  Deriving.add name ~str_type_decl
;;

let stable_variant =
  let name = Naming.stable_variant in
  let str_type_decl = Deriving.Generator.make args (gen name) ~deps:[] in
  Deriving.add name ~str_type_decl
;;
