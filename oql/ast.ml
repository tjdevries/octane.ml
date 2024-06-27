type loc = Lexing.position

let pp_loc fmt (loc : loc) =
  Format.fprintf fmt "<%d:%d>" loc.pos_lnum loc.pos_cnum
;;

type select_statement =
  { expressions : expression list
  ; relation : string option
  ; where : expression option
  }

and table = string

and number =
  | Integer of int
  | Numeric of float

and sql_str = SingleQuote of string

and identifier =
  | Unquoted of string
  | Quoted of string
  | Module of string

and correlation =
  | Table of identifier
  (* schema.nested.thing.table *)
  | Schema of identifier list * identifier
  | SelectionExpression of expression

and index =
  | Specific of expression
  | Slice of expression * expression

and field =
  | Field of loc * loc * identifier
  | Star

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
  | Identifier of identifier
  | String of sql_str
  | BitString of string
  | Number of number
  | TypeCast of (identifier * sql_str)
  | PositionalParam of int
  | NamedParam of string
  | ColumnReference of correlation * field
  | Index of expression * index
  | BinaryExpression of expression * binop * expression
  | UnaryExpression of unop * expression
  | FunctionCall of identifier * expression list
  | Null

and t = Select of select_statement [@@deriving show { with_path = false }]
