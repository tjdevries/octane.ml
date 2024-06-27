{
open Parser
}

(* SQL identifiers and key words must begin with a letter (a-z, but also letters with diacritical marks and non-Latin letters) or an underscore (_). Subsequent characters in an identifier or key word can be letters, underscores, digits (0-9), or dollar signs ($). Note that dollar signs are not allowed in identifiers according to the letter of the SQL standard, so their use might render applications less portable. The SQL standard will not define a key word that contains digits or starts or ends with an underscore, so identifiers of this form are safe against possible conflict with future extensions of the standard. *)
let identifier = ['a'-'z']['a'-'z' 'A'-'Z' '_' '0'-'9' '$']*

(* OCaml Modules are going to be listed like `User.name` - so we're going to differentiate between
   those in our oql *)
let m = ['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*

let quoted_identifier = '"' [^'"']* '"'

let white = [' ' '\t' '\n']+
let select = ['s' 'S']['e' 'E']['l' 'L']['e' 'E']['c' 'C']['t' 'T']
let from = ['f' 'F']['r' 'R']['o' 'O']['m' 'M']
let where = ['w' 'W']['h' 'H']['e' 'E']['r' 'R']['e' 'E']
let null = ['n' 'N']['u' 'U']['l' 'L']['l' 'L']
let cast = ['c' 'C']['a' 'A']['s' 'S']['t' 'T']
let as_ = ['a' 'A']['s' 'S']


(* the sql string regex *)
(* TODO: Strings that are separated by at least one new line are concatentated into a single string *)
(* TODO: C-style escape sequences: https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-STRINGS-ESCAPE *)
(* TODO: Unicode escape strings: https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-STRINGS-UESCAPE *)
let string = '\'' [^'\'']* '\''

(* TODO: Dollar quoted string contants: https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-DOLLAR-QUOTING *)
(* $SOMETHING$ inside of here is a string $SOMETHING$ *)

(* TODO: Bit Strings https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-BIT-STRINGS *)
let bitstring = 'B' '\'' ['0' '1']* '\''

(* spefication for numbers:
  digits
  digits.[digits][e[+-]digits]
  [digits].digits[e[+-]digits]
  digitse[+-]digits *)

(* TODO: 0xhexdigits 0ooctdigits 0bbindigits *)
let digits = ['0'-'9']['0'-'9' '_']*
let integer = digits
let number = digits '.' digits? ('e' ['+' '-']? digits)?

(* TODO: Custom Operators https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-OPERATORS *)

let positional_param = '$' ['1'-'9']['0'-'9']*
let named_param = '$' ['a'-'z']+

(* TODO: Arrays, but they are in chaper 8 *)

(* TODO: Multi-line C-style comments *)
let comment = '-' '-' [^'\n']* '\n'


rule read = 
  parse 
  | white { read lexbuf }
  | "," { COMMA }
  | "." { DOT }
  | ";" { SEMICOLON }
  | "::" { DOUBLE_COLON }
  | ":" { COLON }
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "=" { EQ }
  | positional_param { 
      let position = Lexing.lexeme lexbuf in
      let position = int_of_string (String.sub position 1 (String.length position - 1)) in
      POSITIONAL_PARAM position
  }
  | named_param { 
      let position = Lexing.lexeme lexbuf in
      let position = String.sub position 1 (String.length position - 1) in
      NAMED_PARAM position
  }
  | comment { COMMENT }
  | null { NULL }
  | select { SELECT }
  | from { FROM }
  | where { WHERE }
  | cast { CAST }
  | as_ { AS }
  | string {
        let quoted_string = Lexing.lexeme lexbuf in
        STRING (String.sub quoted_string 1 (String.length quoted_string - 2))
    }
  | bitstring { BITSTRING (Lexing.lexeme lexbuf) }
  (* TODO: Might have to strip _ from numbers, not sure *)
  | integer { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | number { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | m { MODULE (Lexing.lexeme lexbuf) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | quoted_identifier { 
      let quoted_string = Lexing.lexeme lexbuf in
      IDENTIFIER (String.sub quoted_string 1 (String.length quoted_string - 2))
    }
  | eof { EOF }
