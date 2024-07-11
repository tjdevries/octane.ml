module FieldType = struct
  type t =
    | Integer
    | Text
  [@@deriving show { with_path = false }, eq]

  let to_sql = function
    | Integer -> "INTEGER"
    | Text -> "TEXT"
  ;;
end

module Field = struct
  type t =
    { name : string
    ; ty : FieldType.t
    }
  [@@deriving show { with_path = false }, eq]

  let pp fmt t = Fmt.pf fmt "%a('%s')" FieldType.pp t.ty t.name
end

module Table = struct
  type t =
    { name : string
    ; fields : Field.t list
    }
  [@@deriving show { with_path = false }, eq]

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
