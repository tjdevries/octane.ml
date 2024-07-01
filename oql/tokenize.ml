open Parser

type token = Parser.token =
  | WHERE
  | TRUE
  | TABLE_NAME of (string * string)
  | STRING of string
  | STAR
  | SLASH
  | SIMILAR
  | SEMICOLON
  | SELECT
  | SCHEMA_NAME of (string * string * string)
  | RPAR
  | RBRACKET
  | RBRACE
  | POSITIONAL_PARAM of int
  | PLUS
  | PERCENT
  | NUMBER of float
  | NULL
  | NOTNULL
  | NE
  | NAMED_PARAM of string
  | NAME of string
  | MODULE of string
  | MINUS
  | LT
  | LPAR
  | LIKE
  | LE
  | LBRACKET
  | LBRACE
  | ISNULL
  | IS
  | INTEGER of int
  | IN
  | ILIKE
  | GT
  | GE
  | FROM
  | FALSE
  | EQ
  | EOF
  | DOUBLE_COLON
  | DOT
  | DISTINCT
  | COMMENT
  | COMMA
  | COLON
  | COLLATE
  | CAST
  | BITSTRING of string
  | BETWEEN
  | AT
  | AS
  | ALL
[@@deriving show { with_path = false }]

let lex (s : string) : Parser.token list =
  let lexbuf = Lexing.from_string s in
  let rec loop tokens =
    match Lexer.read lexbuf with
    | EOF -> List.rev (EOF :: tokens)
    | token -> loop (token :: tokens)
  in
  loop []
;;

let print_tokens s =
  let tokens = lex s in
  List.iter (fun token -> print_endline (show_token token)) tokens
;;
