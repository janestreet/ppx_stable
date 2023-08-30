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

(* This is complicated so here's some help on fields:
   - source = fields from record [@@deriving stable_record] attached to
   - target = source + add - remove
   - add:[ a ] = value is gong to come from ~a argument
   - set:[ s ] = value is going to come from ~s argument
   - modify:[ m ] = value is going to come from ~modify_m argument

   In particular:
   fields_from_args = set + (target - source) = set + (add - remove)
   are the fields that we expect to get from ~a and ~s arguments.
*)
let convert_record
  ~loc
  ~fields
  ~source_fields
  ~target_fields
  ~modified_fields
  ~set_fields
  ~source_type
  ~target_type
  ~rec_flag
  ~type_name
  =
  let record_pat =
    let record_pat =
      List.map (Set.to_list source_fields) ~f:(fun name ->
        if Set.mem target_fields name && not (Set.mem set_fields name)
        then Ast_helpers.mk_lident ~loc name, ppat_var ~loc (Located.mk ~loc name)
        else Ast_helpers.mk_lident ~loc name, ppat_any ~loc)
    in
    ppat_record ~loc record_pat Closed
  in
  let fields_from_args = Set.union set_fields (Set.diff target_fields source_fields) in
  let any_recursive = ref false in
  let fields =
    Map.of_alist_exn (module String) (List.map fields ~f:(fun ld -> ld.pld_name.txt, ld))
  in
  let map_if_recursive = create_type_name_map ~loc ~rec_flag ~type_name in
  let target_record =
    let fields =
      List.map (Set.to_list target_fields) ~f:(fun name ->
        let expr =
          if Set.mem modified_fields name
          then (
            let f = evar ~loc (Naming.modify_field name) in
            pexp_apply ~loc f [ Nolabel, evar ~loc name ])
          else if Set.mem fields_from_args name
          then evar ~loc name
          else (
            let ld = Map.find_exn fields name in
            Generic_map.build ~loc ~map:map_if_recursive ld.pld_type (evar ~loc name)
            |> set_any_recursive_and_return_expr ~any_recursive)
        in
        Ast_helpers.mk_lident ~loc name, expr)
    in
    pexp_record ~loc fields None
  in
  let acc =
    match !any_recursive with
    | false ->
      [%expr
        let ([%p record_pat] : [%t source_type]) = _t in
        ([%e target_record] : [%t target_type])]
    | true ->
      [%expr
        let rec [%p pvar ~loc Naming.recurse] =
          fun ([%p record_pat] : [%t source_type]) : [%t target_type] ->
          [%e target_record]
        in
        [%e evar ~loc Naming.recurse] _t]
  in
  let acc =
    Set.fold fields_from_args ~init:acc ~f:(fun acc name ->
      Ast_helpers.mk_pexp_fun ~loc ~name acc)
  in
  let acc =
    Set.fold_right modified_fields ~init:acc ~f:(fun name acc ->
      let name = Naming.modify_field name in
      Ast_helpers.mk_pexp_fun ~loc ~name acc)
  in
  (* we put this argument first to help with record field disambiguation at the use site *)
  [%expr fun (_t : [%t source_type]) -> [%e acc]]
;;

let create_ast_structure_items
  ~loc
  ~fields
  ~add
  ~remove
  ~modify
  ~set
  ~target_type
  ~current_type
  ~rec_flag
  ~type_name
  =
  match target_type with
  | None ->
    Location.raise_errorf
      ~loc
      "%s: missing target version"
      (Naming.ppx ~which_ppx:`Record)
  | Some target_type ->
    let current_fields =
      Set.of_list (module String) (List.map fields ~f:(fun ld -> ld.pld_name.txt))
    in
    Invariants.things_are_known
      ~thing_name:Naming.fields
      ~supposed_to_be:Naming.removed
      ~loc
      ~all:current_fields
      remove;
    Invariants.things_are_known
      ~thing_name:Naming.fields
      ~supposed_to_be:Naming.modified
      ~loc
      ~all:current_fields
      modify;
    Invariants.things_are_known
      ~thing_name:Naming.fields
      ~supposed_to_be:Naming.set
      ~loc
      ~all:current_fields
      set;
    let other_fields = Set.diff (Set.union current_fields add) remove in
    let to_target_name =
      Naming.conversion_function ~dir:`To ~source:type_name ~target:target_type
    in
    let of_target_name =
      Naming.conversion_function ~dir:`Of ~source:type_name ~target:target_type
    in
    let to_target =
      convert_record
        ~loc
        ~fields
        ~source_fields:current_fields
        ~target_fields:other_fields
        ~modified_fields:modify
        ~set_fields:set
        ~target_type
        ~source_type:current_type
        ~rec_flag
        ~type_name
    in
    let of_target =
      convert_record
        ~loc
        ~fields
        ~source_fields:other_fields
        ~target_fields:current_fields
        ~modified_fields:modify
        ~set_fields:set
        ~target_type:current_type
        ~source_type:target_type
        ~rec_flag
        ~type_name
    in
    [ [%stri let [%p pvar ~loc to_target_name] = [%e to_target]]
    ; [%stri let [%p pvar ~loc of_target_name] = [%e of_target]]
    ]
;;
