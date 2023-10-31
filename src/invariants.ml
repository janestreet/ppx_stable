open Base
open Ppxlib

let set_to_string set =
  set
  |> Set.to_list
  |> List.map ~f:(fun field_name -> Printf.sprintf "'%s'" field_name)
  |> String.concat ~sep:", "
;;

let all_disjoints ~loc ~add ~remove ~modify ~set =
  let check ?suggestion (n1, s1) (n2, s2) =
    let common = Set.inter s1 s2 in
    if not (Set.is_empty common)
    then
      Location.raise_errorf
        ~loc
        "Sets '%s' and '%s' must be disjoint but they are not: %s found in both%s"
        n1
        n2
        (set_to_string common)
        (Option.value_map suggestion ~default:"" ~f:(fun suggestion -> ". " ^ suggestion))
  in
  let a = "add", add in
  let b = "remove", remove in
  let c = "modify", modify in
  let d = "set", set in
  check a b ~suggestion:"Consider ~modify or ~set";
  check a c;
  check a d;
  check b c;
  check b d;
  check c d
;;

let things_are_known ~loc ~all ~thing_name ~supposed_to_be things =
  let unknown_things = Set.diff things all in
  if not (Set.is_empty unknown_things)
  then (
    let str = set_to_string unknown_things in
    Location.raise_errorf
      ~loc
      "Some %s were supposed to be %s but they were not found: %s"
      thing_name
      supposed_to_be
      str)
;;
