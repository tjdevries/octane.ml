open Base
open State

type t =
  | Table of table_operation
  | Field of field_operation

and table_operation =
  | Create of Table.t
  | Drop of Table.t

and field_operation =
  | Add of table_name * Field.t * FieldType.t
  | Drop of table_name * Field.t
  | Rename of table_name * Field.t * Field.t
  | Alter of table_name * Field.t * FieldType.t

and table_name = string [@@deriving show { with_path = false }]

let create_fields fields =
  let create_field (field : Field.t) =
    Fmt.str "%s %s" field.name (FieldType.to_sql field.ty)
  in
  List.map fields ~f:create_field |> String.concat ~sep:", "
;;

let rec to_sql = function
  | Table table -> table_to_sql table
  | Field field -> field_to_sql field

and table_to_sql = function
  | Create table ->
    "CREATE TABLE " ^ Table.name table ^ " (" ^ create_fields table.fields ^ ")"
  | Drop table -> "DROP TABLE " ^ Table.name table

and field_to_sql = function
  | Add (table, field, ty) ->
    "ALTER TABLE "
    ^ table
    ^ " ADD "
    ^ Field.show field
    ^ " "
    ^ FieldType.show ty
  | _ -> assert false
;;
