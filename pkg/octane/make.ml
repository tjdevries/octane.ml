open Core
open Schema

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
        Operations.add_column next_table.name next_field.name next_field.ty)
    in
    let dropped_fields =
      List.filter prev_fields ~f:(fun prev_field ->
        not (List.mem next_fields prev_field ~equal:Field.equal))
      |> List.map ~f:(fun prev_field ->
        Operations.drop_column prev_table.name prev_field.name)
    in
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
    List.fold new_tables ~init:[] ~f:(fun acc t ->
      (Operations.create_table t.name :: Operations.add_columns_from_table t)
      @ acc)
  in
  let drops =
    let prev_tables = prev.tables in
    let next_tables = next.tables in
    let new_tables =
      List.filter prev_tables ~f:(fun t ->
        not (List.mem next_tables t ~equal:Table.equal_name))
    in
    List.map new_tables ~f:(fun t -> Operations.drop_table t.name)
  in
  let field_operations = find_field_operations ~prev ~next in
  creation @ drops @ field_operations
;;

let ty_int = FieldType.make Integer
let ty_text = FieldType.make Text ~nullable:true
let user_table = Table.make "users" ~fields:[ Field.make "id" ~ty:ty_int ]

let user_table_next =
  Table.make
    "users"
    ~fields:[ Field.make "id" ~ty:ty_int; Field.make "name" ~ty:ty_text ]
;;

let execute_test ~prev ~next =
  let operations = find_operations ~prev ~next in
  Fmt.pr "Operations:@.%a@." pp_operations operations;
  Fmt.pr "@.Migrations:@.";
  List.iter operations ~f:(fun operation ->
    Fmt.pr "%s;@." (Operations.to_sql operation))
;;

let%expect_test "finds no operations for empty tables" =
  let prev = { tables = [] } in
  let next = { tables = [] } in
  execute_test ~prev ~next;
  [%expect {|
    Operations:
    []

    Migrations: |}]
;;

let%expect_test "finds no operations for same tables" =
  let prev = { tables = [ user_table ] } in
  let next = { tables = [ user_table ] } in
  execute_test ~prev ~next;
  [%expect {|
    Operations:
    []

    Migrations: |}]
;;

let%expect_test "creates table for new tables" =
  let prev = { tables = [] } in
  let next = { tables = [ user_table ] } in
  execute_test ~prev ~next;
  [%expect
    {|
    Operations:
    [(CreateTable "users");
      (AlterTable
         (AddColumn ("users", "id", { kind = Integer; nullable = false })))
      ]

    Migrations:
    CREATE TABLE "users";
    ALTER TABLE "users" ADD COLUMN "id" INTEGER NOT NULL;|}]
;;

let%expect_test "deletes table for missing tables" =
  let prev = { tables = [ user_table ] } in
  let next = { tables = [] } in
  execute_test ~prev ~next;
  [%expect
    {|
    Operations:
    [(DropTable "users")]

    Migrations:
    DROP TABLE "users"; |}]
;;

let%expect_test "Adds fields when a new field is added" =
  let prev = { tables = [ user_table ] } in
  let next = { tables = [ user_table_next ] } in
  execute_test ~prev ~next;
  [%expect
    {|
    Operations:
    [(AlterTable (AddColumn ("users", "name", { kind = Text; nullable = true })))
      ]

    Migrations:
    ALTER TABLE "users" ADD COLUMN "name" TEXT; |}]
;;

let%expect_test "Removes fields when a field is removed" =
  let prev = { tables = [ user_table_next ] } in
  let next = { tables = [ user_table ] } in
  execute_test ~prev ~next;
  [%expect
    {|
    Operations:
    [(AlterTable (DropColumn ("users", "name")))]

    Migrations:
    ALTER TABLE "users" DROP COLUMN "name"; |}]
;;

let%expect_test "creates table for new tables with multiple columns" =
  let prev = { tables = [] } in
  let next = { tables = [ user_table_next ] } in
  execute_test ~prev ~next;
  [%expect
    {|
    Operations:
    [(CreateTable "users");
      (AlterTable
         (AddColumn ("users", "id", { kind = Integer; nullable = false })));
      (AlterTable (AddColumn ("users", "name", { kind = Text; nullable = true })))
      ]

    Migrations:
    CREATE TABLE "users";
    ALTER TABLE "users" ADD COLUMN "id" INTEGER NOT NULL;
    ALTER TABLE "users" ADD COLUMN "name" TEXT;|}]
;;
