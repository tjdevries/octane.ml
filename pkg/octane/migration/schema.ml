module FieldType = struct
  type kind =
    | Integer
    | Text

  and t =
    { kind : kind
    ; nullable : bool
    }
  [@@deriving show { with_path = false }, eq]

  let make ?(nullable = false) kind = { kind; nullable }

  let to_sql { kind; nullable } =
    let base =
      match kind with
      | Integer -> "INTEGER"
      | Text -> "TEXT"
    in
    let nullable = if nullable then "" else " NOT NULL" in
    base ^ nullable
  ;;
end

(* module FieldConstraint = struct *)
(*   type t = PrimaryKey of string [@@deriving show { with_path = false }, eq] *)
(* end *)

module Field = struct
  type t =
    { name : string
    ; ty : FieldType.t
    }
  [@@deriving show { with_path = false }, eq]

  let make ~ty name = { name; ty }
  let pp fmt t = Fmt.pf fmt "%a('%s')" FieldType.pp t.ty t.name
end

module TableConstraint = struct
  type t =
    | PrimaryKey of string list
    | ForeignKey of
        { name : string
        ; field : string
        ; foreign_table : string
        ; foreign_field : string
        }
    | Unique of string
  [@@deriving show { with_path = false }, eq]
end

module Table = struct
  type t =
    { name : string
    ; fields : Field.t list
    ; constraints : TableConstraint.t list
    }
  [@@deriving show { with_path = false }, eq]

  let make ?(constraints = []) name ~fields = { name; fields; constraints }
  let name { name; _ } = name

  let equal_name { name = name1; _ } { name = name2; _ } =
    String.equal name1 name2
  ;;

  let equal_fields { fields = fields1; _ } { fields = fields2; _ } =
    List.equal Field.equal fields1 fields2
  ;;
end

(*  *)
type t = { tables : Table.t list } [@@deriving show { with_path = false }]
