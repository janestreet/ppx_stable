open Base
open Ppxlib
open Ast_builder.Default

let create_type_name_map ~loc ~rec_flag ~type_name =
  match rec_flag with
  | Recursive -> Map.singleton (module String) type_name (evar ~loc Naming.recurse)
  | Nonrecursive -> Map.empty (module String)
;;

(* we only need to mark the function recursive if we made any recursive calls. the type
   might say recursive (the default) without actually being recursive *)
let set_any_recursive_and_return_expr ~any_recursive (result, expr) =
  match (result : Generic_map.replace_result) with
  | Unchanged -> expr
  | Replaced ->
    any_recursive := true;
    expr
;;

module Constructor = struct
  type t =
    { name : label
    ; args : constructor_arguments
    }
end

module Info = struct
  type t =
    { constructors : Constructor.t list
    ; is_polymorphic : [ `Polymorphic | `Not_polymorphic ]
    ; type_name : string
    }

  let of_cdl ~type_name cdl =
    let constructors =
      List.map cdl ~f:(fun cd ->
        let name = cd.pcd_name.txt in
        { Constructor.name; args = cd.pcd_args })
    in
    { constructors; is_polymorphic = `Not_polymorphic; type_name }
  ;;

  let of_row_fields ~loc ~ppx_name ~type_name rfs =
    let rfds = List.map rfs ~f:(fun { prf_desc; _ } -> prf_desc) in
    let constructors =
      List.map rfds ~f:(function
        | Rtag (loc_and_name, true, []) ->
          let name = loc_and_name.txt in
          let args = Pcstr_tuple [] in
          { Constructor.name; args }
        (* In normal variants, arguments to a constructor "| A (int * bool)" are
           stored in a "Pcstr_tuple core_type_list". For polymorphic variants,
           arguments are stored in a core_type_list, because there are more
           type possibilities. "|`A of int & bool" has arguments [int; bool].
           "|`A of (int * bool)" has its arguments stored as
           [pstr_tuple [int; bool]]. "|`A of int" has arguments [int]. Here we
           translate one argument and "pstr_tuple"-d arguments to "Pcstr_tuple",
           and fail on the unsupported multiple arguments case.
        *)
        | Rtag (loc_and_name, false, [ t ]) ->
          let name = loc_and_name.txt in
          let args =
            Pcstr_tuple
              (match t.ptyp_desc with
               | Ptyp_tuple core_type_list -> core_type_list
               | Ptyp_any
               | Ptyp_var _
               | Ptyp_arrow _
               | Ptyp_constr _
               | Ptyp_object _
               | Ptyp_class _
               | Ptyp_alias _
               | Ptyp_variant _
               | Ptyp_poly _
               | Ptyp_package _
               | Ptyp_extension _ -> [ t ])
          in
          { Constructor.name; args }
        | Rtag (_, _, args) ->
          let args_str =
            args |> List.map ~f:string_of_core_type |> String.concat ~sep:" & "
          in
          Location.raise_errorf
            ~loc
            "%s: Some polymorphic variants not yet supported. This polymorphic variant \
             has an AST type that this ppx doesn't understand: %s"
            ppx_name
            args_str
        | Rinherit _ ->
          Location.raise_errorf
            ~loc
            "%s: polymorphic variants extending other variants not yet supported"
            ppx_name)
    in
    { constructors; is_polymorphic = `Polymorphic; type_name }
  ;;

  let names { constructors; _ } =
    List.map ~f:(fun { Constructor.name; _ } -> name) constructors
  ;;

  let to_map { constructors; _ } =
    constructors
    |> List.map ~f:(fun ({ Constructor.name; _ } as t) -> name, t)
    |> Map.of_alist_exn (module String)
  ;;

  let to_list { constructors; _ } = constructors
end

let stable_variants ~type_ =
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = type_; _ }, _) ->
    let type_name = List.hd_exn (List.rev (Longident.flatten_exn type_)) in
    Naming.stable_variant_module ~type_name
  | _ -> assert false
;;

let constructor_args_and_pattern_of_args ~loc ~tuple_opt ~record ~f ~constructor_arguments
  =
  match constructor_arguments with
  | Pcstr_tuple tys ->
    let args, pats =
      List.mapi tys ~f:(fun i ty ->
        let var = "v" ^ Int.to_string i in
        (Nolabel, var), f ty var)
      |> List.unzip
    in
    args, tuple_opt pats
  | Pcstr_record lds ->
    let args, pats =
      List.mapi lds ~f:(fun i ld ->
        let var = "v" ^ Int.to_string i in
        ( (Labelled ld.pld_name.txt, var)
        , (Located.lident ~loc ld.pld_name.txt, f ld.pld_type var) ))
      |> List.unzip
    in
    args, Some (record pats)
;;

let generate_stable_variant_module ~loc ~variant_info =
  let alias_fun_label name = String.lowercase name ^ "_fun" in
  let variant_constructors = Info.to_list variant_info in
  let map_function =
    let cases =
      List.map
        variant_constructors
        ~f:(fun { Constructor.name = constructor_name; args = constructor_arguments } ->
        let args, pattern =
          constructor_args_and_pattern_of_args
            ~constructor_arguments
            ~loc
            ~tuple_opt:(ppat_tuple_opt ~loc)
            ~record:(fun p -> ppat_record ~loc p Closed)
            ~f:(fun _ x -> pvar ~loc x)
        in
        let pattern =
          match variant_info.is_polymorphic with
          | `Polymorphic -> ppat_variant ~loc constructor_name pattern
          | `Not_polymorphic ->
            ppat_construct ~loc (Located.lident ~loc constructor_name) pattern
        in
        let value =
          let fun_expr = evar ~loc (alias_fun_label constructor_name) in
          if List.is_empty args
          then [%expr [%e fun_expr] ()]
          else
            List.map args ~f:(fun (lbl, x) -> lbl, evar ~loc x)
            |> pexp_apply ~loc fun_expr
        in
        case ~guard:None ~lhs:pattern ~rhs:value)
    in
    let expr =
      List.fold_right
        ~init:(pexp_function ~loc cases)
        variant_constructors
        ~f:(fun { Constructor.name; _ } acc ->
        let name = String.lowercase name in
        pexp_fun ~loc (Labelled name) None (pvar ~loc (alias_fun_label name)) acc)
    in
    Ast_helpers.mk_module
      ~loc
      ~name:Naming.helper_module
      ~items:
        [ pstr_value
            ~loc
            Nonrecursive
            [ value_binding ~loc ~pat:(pvar ~loc Naming.map) ~expr ]
        ]
  in
  [ Ast_helpers.mk_module
      ~loc
      ~name:(Naming.stable_variant_module ~type_name:variant_info.type_name)
      ~items:[ map_function ]
  ]
;;

let convert_variant
  ~loc
  ~(variant_info : Info.t)
  ~source_variants
  ~target_variants
  ~modified_variants
  ~target_type
  ~source_type
  ~rec_flag
  =
  (* Create pexp_ident scoped to the same module as [which_type]. *)
  let variants_longident ~loc ~which_type path =
    let add longident_opt x =
      match longident_opt with
      | Some l -> Some (Ldot (l, x))
      | None -> Some (Lident x)
    in
    let init =
      match which_type.ptyp_desc with
      | Ptyp_constr (lid_loc, _) ->
        (match lid_loc.txt with
         | Lapply _ -> Location.raise_errorf ~loc "Unexpected Lapply"
         | Lident _ -> None
         | Ldot (t, _) -> Some t)
      | _ -> assert false
    in
    let longident = Option.value_exn (List.fold ~init path ~f:add) in
    Located.mk ~loc longident
  in
  let any_recursive = ref false in
  let map_if_recursive =
    create_type_name_map ~loc ~rec_flag ~type_name:variant_info.type_name
  in
  let args_to_constructor_function ~variant_constructor:{ Constructor.name; args } =
    let args, value =
      constructor_args_and_pattern_of_args
        ~constructor_arguments:args
        ~loc
        ~tuple_opt:(pexp_tuple_opt ~loc)
        ~record:(fun e -> pexp_record ~loc e None)
        ~f:(fun ty alias ->
          Generic_map.build ~loc ~map:map_if_recursive ty (evar ~loc alias)
          |> set_any_recursive_and_return_expr ~any_recursive)
    in
    let value =
      match variant_info.is_polymorphic with
      | `Polymorphic -> pexp_variant ~loc name value
      | `Not_polymorphic ->
        pexp_construct
          ~loc
          (variants_longident ~which_type:target_type ~loc [ name ])
          value
    in
    if List.is_empty args
    then [%expr fun () -> [%e value]]
    else
      List.fold_right args ~init:value ~f:(fun (label, alias) acc ->
        pexp_fun ~loc label None (pvar ~loc alias) acc)
  in
  let acc =
    let map_fn =
      variants_longident
        ~loc
        ~which_type:source_type
        [ stable_variants ~type_:source_type; Naming.helper_module; Naming.map ]
    in
    [%expr [%e pexp_ident ~loc map_fn] v]
  in
  let variant_constructors = Info.to_map variant_info in
  let rhs =
    Set.fold source_variants ~init:acc ~f:(fun acc name ->
      let f =
        if Set.mem modified_variants name
        then evar ~loc (Naming.modify_field name)
        else if not (Set.mem target_variants name)
        then evar ~loc (Naming.remove_field name)
        else (
          let variant_constructor = Map.find_exn variant_constructors name in
          args_to_constructor_function ~variant_constructor)
      in
      pexp_apply ~loc acc [ Labelled (String.lowercase name), f ])
  in
  let acc =
    match !any_recursive with
    | false -> [%expr fun (v : [%t source_type]) : [%t target_type] -> [%e rhs]]
    | true ->
      [%expr
        let rec [%p pvar ~loc Naming.recurse] =
          fun (v : [%t source_type]) : [%t target_type] -> [%e rhs]
        in
        [%e evar ~loc Naming.recurse]]
  in
  let acc =
    Set.fold (Set.diff source_variants target_variants) ~init:acc ~f:(fun acc name ->
      let name = Naming.remove_field name in
      Ast_helpers.mk_pexp_fun ~loc ~name acc)
  in
  let acc =
    Set.fold_right modified_variants ~init:acc ~f:(fun name acc ->
      let name = Naming.modify_field name in
      Ast_helpers.mk_pexp_fun ~loc ~name acc)
  in
  acc
;;

let conversions_of_variant
  ~loc
  ~add
  ~modify
  ~remove
  ~set
  ~target_type
  ~current_type
  ~rec_flag
  ~variant_info
  =
  let current_variants = Set.of_list (module String) (Info.names variant_info) in
  Invariants.things_are_known
    ~thing_name:Naming.variants
    ~supposed_to_be:Naming.removed
    ~loc
    ~all:current_variants
    remove;
  Invariants.things_are_known
    ~thing_name:Naming.variants
    ~supposed_to_be:Naming.modified
    ~loc
    ~all:current_variants
    modify;
  if not (Set.is_empty set) then Location.raise_errorf ~loc "[set] is for record only";
  let other_variants = Set.diff (Set.union current_variants add) remove in
  let to_target =
    convert_variant
      ~loc
      ~variant_info
      ~source_variants:current_variants
      ~target_variants:other_variants
      ~modified_variants:modify
      ~target_type
      ~source_type:current_type
      ~rec_flag
  in
  let of_target =
    convert_variant
      ~loc
      ~variant_info
      ~source_variants:other_variants
      ~target_variants:current_variants
      ~modified_variants:modify
      ~target_type:current_type
      ~source_type:target_type
      ~rec_flag
  in
  to_target, of_target
;;

let create_ast_structure_items
  ~loc
  ~add
  ~modify
  ~remove
  ~set
  ~target_type
  ~current_type
  ~rec_flag
  ~(variant_info : Info.t)
  =
  let conversions =
    match target_type with
    | None -> []
    | Some target_type ->
      let to_target_name =
        Naming.conversion_function
          ~dir:`To
          ~source:variant_info.type_name
          ~target:target_type
      in
      let of_target_name =
        Naming.conversion_function
          ~dir:`Of
          ~source:variant_info.type_name
          ~target:target_type
      in
      let to_target, of_target =
        conversions_of_variant
          ~loc
          ~add
          ~modify
          ~remove
          ~set
          ~target_type
          ~current_type
          ~rec_flag
          ~variant_info
      in
      [ [%stri let [%p pvar ~loc to_target_name] = [%e to_target]]
      ; [%stri let [%p pvar ~loc of_target_name] = [%e of_target]]
      ]
  in
  let helper_module = generate_stable_variant_module ~loc ~variant_info in
  helper_module @ conversions
;;
