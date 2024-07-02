%{
open Ast
%}

(* Good test cases
   - https://github.com/pganalyze/libpg_query/blob/16-latest/test/sql/postgres_regress/aggregates.sql
   - https://github.com/tobymao/sqlglot/tree/main/tests*)

%token <string> MODULE
%token <string> STRING
%token <string> BITSTRING
%token <int> INTEGER
%token <float> NUMBER
%token <int> POSITIONAL_PARAM
%token <string> NAMED_PARAM

%token <string> NAME
%token <string * string> TABLE_NAME
%token <string * string * string> SCHEMA_NAME

%token COMMENT

(* Select Keywords *)
%token DISTINCT ALL

(* operators *)
%token PLUS MINUS SLASH PERCENT
%token COLLATE AT
%token GT GE LT LE EQ NE
%token BETWEEN IN LIKE ILIKE SIMILAR
%token IS ISNULL NOTNULL

%token SELECT FROM CAST AS WHERE
%token STAR NULL TRUE FALSE
%token COMMA DOT SEMICOLON COLON DOUBLE_COLON
%token LPAR RPAR LBRACKET RBRACKET LBRACE RBRACE
%token EOF

(* Joins *)
%token LEFT RIGHT FULL OUTER INNER CROSS NATURAL JOIN
%token ON USING

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

let name :=
  | name = NAME; { $startpos, $endpos, name }

let query :=
  | SELECT; ~ = select; from = from?; where = where?; SEMICOLON?; EOF;
      { Select { select; from; where } }

let select :=
  | result_kind = result_kind?; ~ = result_columns; { { result_kind; result_columns } }

let result_kind :=
  | DISTINCT; { Distinct }
  | ALL; { All }

let result_columns :=
  | results = separated_nonempty_list(COMMA, result_column); { results }

let result_column :=
  | STAR; { Star }
  | expression = expression; { Expression (expression, None) }

let from := 
  | FROM; relation = separated_nonempty_list(COMMA, table_or_subquery); { From relation }
  | FROM; relation = table_or_subquery; stanzas = join_stanza+; { Join { relation; stanzas } }
  (* | FROM;  *)

let where :=
  | WHERE; ~ = expression; { expression }

let join_stanza :=
  | ~ = join_operator; ~ = table_or_subquery; ~ = join_constraint; { join_operator, table_or_subquery, join_constraint }

let join_operator := 
  | LEFT; OUTER; JOIN; { LeftOuter }
  | INNER; JOIN; { Inner }
  (* ... *)

let join_constraint :=
  | ON; ~ = expression; { On expression }
  | USING; LPAR; column_names = separated_list(COMMA, column_name); RPAR; { Using column_names }

let table_or_subquery :=
  | m = MODULE; { Model (Model.make ($startpos, $endpos, m)) }
  | ~ = name; { Table (Table.make name) }
  (* | t = IDENTIFIER; { Table (Table.of_string t) } *)

let expression :=
  | ~ = number; <NumericLiteral>
  | ~ = string; <StringLiteral>
  | NULL; { Null }
  | TRUE; { BooleanLiteral true }
  | FALSE; { BooleanLiteral false }
  | column = column; { column }
  | ~ = BITSTRING; <BitString>
  (* | typecast *)
  | ~ = POSITIONAL_PARAM; <PositionalParam>
  | ~ = NAMED_PARAM; <NamedParam>
  (* | delimited(LPAR, expression, RPAR) *)
  (* | ~ = expression; LBRACKET; ~ = index; RBRACKET; <Index> *)
  | binop
  | unop
  | function_call

let column_name :=
  | ~ = name; { 
    let field = Field.make name in
    Column.make None None field
  }

let field :=
  | ~ = column_name; <Column>
  | m = model; DOT; name = name; {
    ModelField (ModelField.make m name)
  }

let model := 
  | m = MODULE; { $startpos, $endpos, m }

let column :=
  | ~ = field; { field }
  | ~ = schema; { schema }
  | ~ = table; { table }

let schema := 
  | schema = name; DOT; table = name; DOT; field = name; {
    let schema = Schema.make schema in
    let table = Table.make table in
    let field = Field.make field in
    Column (Column.make (Some schema) (Some table) field)
  }

let table := 
  | table = name; DOT; field = name; {
    let table = Table.make table in
    let field = Field.make field in
    Column (Column.make None (Some table) field)
  }

let binop :=
  | (left, right) = binoprule(PLUS); { BinaryExpression (left, Add, right) }
  | (left, right) = binoprule(MINUS); { BinaryExpression (left, Sub, right) }
  | (left, right) = binoprule(STAR); { BinaryExpression (left, Mul, right) }
  | (left, right) = binoprule(SLASH); { BinaryExpression (left, Div, right) }
  | (left, right) = binoprule(EQ); { BinaryExpression (left, Eq, right) }
  | (left, right) = binoprule(GT); { BinaryExpression (left, Gt, right) }
  | (left, right) = binoprule(GE); { BinaryExpression (left, Gt, right) }
  | (left, right) = binoprule(LT); { BinaryExpression (left, Lt, right) }
  | (left, right) = binoprule(LE); { BinaryExpression (left, Lte, right) }
  | (left, right) = binoprule(PERCENT); { BinaryExpression (left, Mod, right) }

let binoprule(middle) ==
  | left = expression; _ = middle; right = expression; { left, right }

let unop :=
  | ~ = MINUS; ~ = expression; { UnaryExpression (Neg, expression) }
  | ~ = PLUS; ~ = expression; { UnaryExpression (Pos, expression) }

let function_call :=
  | ~ = func_name; args = delimited(LPAR, separated_list(COMMA, expression), RPAR); <FunctionCall>

let func_name := 
  | ~ = name; { FuncName.make name }

let type_name := 
  | ~ = name; { TypeName.make name }

let string :=
  | ~ = STRING; <SingleQuote>

let number := 
  | ~ = INTEGER; <Integer>
  | ~ = NUMBER; <Numeric>

(* https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-CONSTANTS-GENERIC *)
let typecast := 
  | ~ = type_name; ~ = string; <TypeCast>
  | ~ = string; DOUBLE_COLON; ~ = name; { TypeCast (TypeName.make name, string) }
  (* | CAST; LPAR; ~ = string; AS; ~ = identifier; RPAR; { TypeCast (identifier, string) } *)

let index :=
  | ~ = expression; <Specific>
  | start = expression; COLON; stop = expression; { Slice (start, stop) }

(* aggregate_name (expression [ , ... ] [ order_by_clause ] ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name (ALL expression [ , ... ] [ order_by_clause ] ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name (DISTINCT expression [ , ... ] [ order_by_clause ] ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name ( * ) [ FILTER ( WHERE filter_clause ) ] *)
(* aggregate_name ( [ expression [ , ... ] ] ) WITHIN GROUP ( order_by_clause ) [ FILTER ( WHERE filter_clause ) ] *)
