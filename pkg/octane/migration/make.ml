open Core
open State

type operations = Operations.t list [@@deriving show { with_path = false }]

let find_modified_tables ~prev ~next =
  let prev_tables = prev.tables in
  let next_tables = next.tables in
  let modified_tables =
    List.filter_map prev_tables ~f:(fun prev_table ->
      match
        List.find next_tables ~f:(fun next_table ->
          Table.equal_name prev_table next_table
          && not (Table.equal_fields prev_table next_table))
      with
      | Some next_table -> Some (prev_table, next_table)
      | None -> None)
  in
  modified_tables
;;

let find_field_operations ~prev ~next =
  let modified = find_modified_tables ~prev ~next in
  List.fold_left modified ~init:[] ~f:(fun acc (prev_table, next_table) ->
    let prev_fields = prev_table.fields in
    let next_fields = next_table.fields in
    let added_fields =
      List.filter next_fields ~f:(fun next_field ->
        not (List.mem prev_fields next_field ~equal:Field.equal))
      |> List.map ~f:(fun next_field ->
        Operations.Field
          (Operations.Add (next_table.name, next_field, next_field.ty)))
    in
    let dropped_fields =
      List.filter prev_fields ~f:(fun prev_field ->
        not (List.mem next_fields prev_field ~equal:Field.equal))
      |> List.map ~f:(fun prev_field ->
        Operations.Field (Operations.Drop (prev_table.name, prev_field)))
    in
    (* let renamed_fields = *)
    (*   List.filter next_fields ~f:(fun next_field -> *)
    (*     List.exists prev_fields ~f:(fun prev_field -> *)
    (*       Field.equal prev_field next_field *)
    (*       && not (Field.equal prev_field.name next_field.name))) *)
    (*   |> List.map ~f:(fun next_field -> *)
    (*     Operations.Field *)
    (*       (Operations.Rename (next_table, next_field, next_field.name))) *)
    (* in *)
    (* let modified_fields = *)
    (*   List.filter next_fields ~f:(fun next_field -> *)
    (*     List.filter next_fields ~f:(fun next_field -> *)
    (*       List.exists prev_fields ~f:(fun prev_field -> *)
    (*         Field.equal prev_field next_field *)
    (*         && not (Field.equal prev_field.name next_field.name)))) *)
    (*   |> List.map ~f:(fun next_field -> *)
    (*     Operations.Field *)
    (*       (Operations.Modify (next_table, next_field, next_field.ty))) *)
    (* in *)
    acc @ added_fields @ dropped_fields)
;;

let find_operations ~prev ~next =
  let creation =
    let prev_tables = prev.tables in
    let next_tables = next.tables in
    let new_tables =
      List.filter next_tables ~f:(fun t ->
        not (List.mem prev_tables t ~equal:Table.equal_name))
    in
    List.map new_tables ~f:(fun t -> Operations.Table (Operations.Create t))
  in
  let drops =
    let prev_tables = prev.tables in
    let next_tables = next.tables in
    let new_tables =
      List.filter prev_tables ~f:(fun t ->
        not (List.mem next_tables t ~equal:Table.equal_name))
    in
    List.map new_tables ~f:(fun t -> Operations.Table (Operations.Drop t))
  in
  let field_operations = find_field_operations ~prev ~next in
  creation @ drops @ field_operations
;;

let user_table : Table.t =
  { name = "users"; fields = [ { name = "id"; ty = Integer } ] }
;;

let user_table_next : Table.t =
  { name = "users"
  ; fields = [ { name = "id"; ty = Integer }; { name = "name"; ty = Text } ]
  }
;;

let execute_test ~prev ~next =
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a@." pp_operations operations;
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a@." pp_operations operations
;;

let%expect_test "finds no operations for empty tables" =
  let prev = { tables = [] } in
  let next = { tables = [] } in
  execute_test ~prev ~next;
  [%expect {|
    []
    [] |}]
;;

let%expect_test "finds no operations for same tables" =
  let prev = { tables = [ user_table ] } in
  let next = { tables = [ user_table ] } in
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a@." pp_operations operations;
  [%expect {| [] |}]
;;

let%expect_test "creates table for new tables" =
  let prev = { tables = [] } in
  let next = { tables = [ user_table ] } in
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a" pp_operations operations;
  [%expect
    {|
    [(Table (Create { name = "users"; fields = [Integer('id')] }))] |}];
  List.iter operations ~f:(fun operation ->
    Fmt.pr "%s;@." (Operations.to_sql operation));
  [%expect {| CREATE TABLE users (id INTEGER); |}]
;;

(* Fmt.pr "%a" Fmt.list ~sep:Fmt.cr Operations.to_sql *)

let%expect_test "deletes table for missing tables" =
  let prev = { tables = [ user_table ] } in
  let next = { tables = [] } in
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a@." pp_operations operations;
  [%expect
    {|
    [(Table (Drop { name = "users"; fields = [Integer('id')] }))] |}]
;;

let%expect_test "Adds fields when a new field is added" =
  let prev = { tables = [ user_table ] } in
  let next = { tables = [ user_table_next ] } in
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a@." pp_operations operations;
  [%expect {| [(Field (Add ("users", Text('name'), Text)))] |}]
;;

let%expect_test "Removes fields when a field is removed" =
  let prev = { tables = [ user_table_next ] } in
  let next = { tables = [ user_table ] } in
  let operations = find_operations ~prev ~next in
  Fmt.pr "%a@." pp_operations operations;
  [%expect {| [(Field (Drop ("users", Text('name'))))] |}]
;;
