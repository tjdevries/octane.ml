open Base

type loc = Lexing.position

let pp_loc fmt (loc : loc) =
  Stdlib.Format.fprintf fmt "<%d:%d>" loc.pos_lnum loc.pos_cnum
;;

(* TODO: This is a bit weird, but it's OK for now. Used to ignore locs when comparing equality *)
let equal_loc a b = true

module Name = struct
  type t = loc * loc * string [@@deriving show, eq]

  let make (start, finish, s) : t = start, finish, s
end

module MakeName () = struct
  type t = Name.t [@@deriving show, eq]

  let make s = s
  let start (start, _, _) = start
  let finish (_, finish, _) = finish
  let name (_, _, s) = s
end

module Schema = MakeName ()
module Table = MakeName ()
module Module = MakeName ()
module FuncName = MakeName ()
module TypeName = MakeName ()

module Field = struct
  include MakeName ()
end

module Column = struct
  type t =
    { schema : Schema.t option
    ; table : Table.t option
    ; field : Field.t
    }
  [@@deriving show, eq]

  let make schema table field = { schema; table; field }
end

type select_statement =
  { select : select_clause
  ; from : from_clause option
  ; where : where_clause option
  }

and select_clause =
  { result_kind : result_kind option
  ; result_columns : result_column list
  }

and from_clause =
  { relation : table_or_subquery list
  ; join : join_clause option
  }

and where_clause = expression

and join_clause =
  { relation : table_or_subquery
  ; stanza : (join_operator * table_or_subquery * join_constraint) list
  }

(* TODO: table-or-subquery *)
and table_or_subquery =
  | Table of Table.t
  | Module of Module.t
  | Subquery of string

and join_operator =
  (* Could do natural, but KEKW *)
  | Left
  | LeftOuter
  | Right
  | RightOuter
  | Full
  | FullOuter
  | Inner
  | Cross

and join_constraint =
  | On of expression
  | Using of column_name list

and result_kind =
  | Distinct
  | All

(* TODO: Thinking about [ `Alias of string ], [`Table of string] ... *)
and alias = string
and table_name = string
and column_name = string

and result_column =
  | Expression of expression * alias option
  | Star
  | TableStar of table_name

and table = string

and numeric_literal =
  | Integer of int
  | Numeric of float

and string_literal = SingleQuote of string

and index =
  | Specific of expression
  | Slice of expression * expression

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | Mod

and unop =
  | Neg
  | Pos

and expression =
  | NumericLiteral of numeric_literal
  | StringLiteral of string_literal
  (* | BlobLiteral of blob_literal *)
  | Null
  | BooleanLiteral of bool
  (* | CurrentDate | CurrentTime | CurrentTimestamp *)
  (* BindParameter *)
  | Column of Column.t
  | TypedColumn of (string * string)
  | BitString of string
  | TypeCast of (TypeName.t * string_literal)
  | PositionalParam of int
  | NamedParam of string
  | Index of expression * index
  | BinaryExpression of expression * binop * expression
  | UnaryExpression of unop * expression
  | FunctionCall of FuncName.t * expression list

and t = Select of select_statement [@@deriving show { with_path = false }, eq]

let get_relation_ident (relation : table_or_subquery) =
  match relation with
  | Table table -> Table.name table
  | Module m -> Module.name m
  | Subquery _ -> failwith "TODO: Subquery"
;;

let get_select_expressions (select : select_clause) =
  List.filter_map select.result_columns ~f:(function
    | Expression (expr, _) -> Some expr
    | _ -> None)
;;
