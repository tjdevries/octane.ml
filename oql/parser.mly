%{
open Ast
%}

(* Good test cases
   - https://github.com/pganalyze/libpg_query/blob/16-latest/test/sql/postgres_regress/aggregates.sql
   - https://github.com/tobymao/sqlglot/tree/main/tests*)

%token <string> MODULE
%token <string> IDENTIFIER
%token <string> STRING
%token <string> BITSTRING
%token <int> INTEGER
%token <float> NUMBER
%token <int> POSITIONAL_PARAM
%token <string> NAMED_PARAM

%token COMMENT

(* operators *)
%token PLUS MINUS SLASH PERCENT
%token COLLATE AT
%token GT GE LT LE EQ NE
%token BETWEEN IN LIKE ILIKE SIMILAR
%token IS ISNULL NOTNULL

%token SELECT FROM CAST AS
%token STAR NULL
%token COMMA DOT SEMICOLON COLON DOUBLE_COLON
%token LPAR RPAR LBRACKET RBRACKET LBRACE RBRACE
%token EOF

(* https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-PRECEDENCE *)
%left OR
%left AND
%right NOT
%nonassoc IS ISNULL NOTNULL
%nonassoc LT LE GT GE EQ NE
%nonassoc BETWEEN IN LIKE ILIKE SIMILAR
%left PLUS MINUS
%left STAR SLASH PERCENT
%left CARET
%left AT
%left COLLATE
(* %right PLUS MINUS (* unary plus/minus *) *)
(* TODO: Not sure how to differentiate these *)
%left LBRACKET
%left DOUBLE_COLON
%left DOT


%start <Ast.t> query
%%

let query :=
  | SELECT; ~ = expressions; FROM; m = MODULE; EOF; { Select { expressions; relation = Some m } }
  | SELECT; ~ = expressions; SEMICOLON?; EOF; { Select { expressions; relation = None } }

let expressions :=
  | separated_list(COMMA, expression)

let expression :=
  | typecast
  | ~ = identifier; <Identifier>
  | ~ = string; <String>
  | ~ = BITSTRING; <BitString>
  | ~ = number; <Number>
  | ~ = POSITIONAL_PARAM; <PositionalParam>
  (* | ~ = INTEGER; <Integer> *)
  (* | ~ = NUMBER; <Number> *)
  | delimited(LPAR, expression, RPAR)
  | ~ = correlation; DOT; ~ = field; <ColumnReference>
  | ~ = expression; LBRACKET; ~ = index; RBRACKET; <Index>
  | binop
  | unop
  | function_call
  | NULL; { Null }

let binop :=
  | (left, right) = binoprule(PLUS); { BinaryExpression (left, Add, right) }
  | (left, right) = binoprule(MINUS); { BinaryExpression (left, Sub, right) }
  | (left, right) = binoprule(STAR); { BinaryExpression (left, Mul, right) }
  | (left, right) = binoprule(SLASH); { BinaryExpression (left, Div, right) }

let binoprule(middle) ==
  | left = expression; _ = middle; right = expression; { left, right }

let unop :=
  | ~ = MINUS; ~ = expression; { UnaryExpression (Neg, expression) }
  | ~ = PLUS; ~ = expression; { UnaryExpression (Pos, expression) }

let function_call :=
  | ~ = identifier; args = delimited(LPAR, separated_list(COMMA, expression), RPAR); <FunctionCall>

let identifier :=
  | ~ = IDENTIFIER; <Unquoted>

let field :=
  | ~ = identifier; { Field ($startpos, $endpos, identifier) }
  | STAR; { Star }

let string :=
  | ~ = STRING; <SingleQuote>

let number := 
  | ~ = INTEGER; <Integer>
  | ~ = NUMBER; <Numeric>

(* https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-CONSTANTS-GENERIC *)
let typecast := 
  | ~ = identifier; ~ = string; <TypeCast>
  | ~ = string; DOUBLE_COLON; ~ = identifier; { TypeCast (identifier, string) }
  | CAST; LPAR; ~ = string; AS; ~ = identifier; RPAR; { TypeCast (identifier, string) }

let correlation :=
  | ~ = identifier; <Table>
  | m = MODULE; { Table (Module m) }

  (* tbh, this doesn't feel great *)
  (* | ~ = MODULE; <SelectionExpression> *)

let index :=
  | ~ = expression; <Specific>
  | start = expression; COLON; stop = expression; { Slice (start, stop) }

(* aggregate_name (expression [ , ... ] [ order_by_clause ] ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name (ALL expression [ , ... ] [ order_by_clause ] ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name (DISTINCT expression [ , ... ] [ order_by_clause ] ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name ( * ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name ( [ expression [ , ... ] ] ) WITHIN GROUP ( order_by_clause ) [ FILTER ( WHERE filter_clause ) ] *)
