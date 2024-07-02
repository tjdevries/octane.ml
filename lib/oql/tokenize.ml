type token = Parser.token =
  | WHERE
  | USING
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
  | RIGHT
  | RBRACKET
  | RBRACE
  | POSITIONAL_PARAM of int
  | PLUS
  | PERCENT
  | OUTER
  | ON
  | NUMBER of float
  | NULL
  | NOTNULL
  | NE
  | NATURAL
  | NAMED_PARAM of string
  | NAME of string
  | MODULE of string
  | MINUS
  | LT
  | LPAR
  | LIKE
  | LEFT
  | LE
  | LBRACKET
  | LBRACE
  | JOIN
  | ISNULL
  | IS
  | INTEGER of int
  | INNER
  | IN
  | ILIKE
  | GT
  | GE
  | FULL
  | FROM
  | FALSE
  | EQ
  | EOF
  | DOUBLE_COLON
  | DOT
  | DISTINCT
  | CROSS
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
