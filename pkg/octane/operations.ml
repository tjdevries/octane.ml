open Core
open Schema

type t =
  | CreateTable of table_name
  | AlterTable of alter_table
  | DropTable of table_name

and alter_table =
  | AddColumn of table_name * field_name * FieldType.t
  | DropColumn of table_name * field_name
(* | AlterColumn| Index *)

and field_operation =
  | Add of table_name * field_name * FieldType.t
  | Drop of table_name * Field.t
  | Rename of table_name * Field.t * Field.t
  | Alter of table_name * Field.t * FieldType.t

and table_name = string
and field_name = string [@@deriving show { with_path = false }]

let create_table table = CreateTable table
let drop_table table = DropTable table
let add_column table field ty = AlterTable (AddColumn (table, field, ty))
let drop_column table field = AlterTable (DropColumn (table, field))

let add_columns_from_table (table : Table.t) =
  List.map table.fields ~f:(fun field ->
    add_column table.name field.name field.ty)
;;

let create_fields fields =
  let create_field (field : Field.t) =
    Fmt.str "%s %s" field.name (FieldType.to_sql field.ty)
  in
  List.map fields ~f:create_field |> String.concat ~sep:", "
;;

let rec to_sql = function
  | CreateTable table -> Fmt.str {|CREATE TABLE "%s"|} table
  | DropTable table -> Fmt.str {|DROP TABLE "%s"|} table
  | AlterTable operation -> alter_table operation

and alter_table = function
  | AddColumn (table, field, ty) ->
    Fmt.str
      {|ALTER TABLE "%s" ADD COLUMN "%s" %s|}
      table
      field
      (FieldType.to_sql ty)
  | DropColumn (table, field) ->
    Fmt.str {|ALTER TABLE "%s" DROP COLUMN "%s"|} table field
;;
