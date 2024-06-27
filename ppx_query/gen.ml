open Ppxlib
open Oql
open Base

let table_relation ~loc relation =
  let ident = Ldot (Lident relation, "relation") in
  let ident = Loc.make ~loc ident in
  Ast_helper.Exp.ident ~loc ident
;;

let rec to_query_string ~loc (ast : Ast.t) =
  let _ = ast in
  match ast with
  | Select select -> to_select_string ~loc select

and to_select_string ~loc (select : Ast.select_statement) =
  match select.relation with
  | Some relation ->
    let table = table_relation ~loc relation in
    let selection = to_expressions_string ~loc select.expressions in
    let e =
      [%expr
        Stdlib.Format.sprintf "SELECT %s FROM %s" [%e selection] [%e table]]
    in
    e
  (* Fmt.str *)
  (*   "SELECT %s FROM %s" *)
  (*   (to_expressions_string select.expressions) *)
  (*   relation *)
  (* | None -> Fmt.str "SELECT %s" (to_expressions_string select.expressions) *)
  | None -> failwith "no relations: must be arch user"

and to_expressions_string ~loc (expressions : Ast.expression list) =
  let exprs = List.map ~f:(to_expression_string ~loc) expressions in
  let exprs = Ast_builder.Default.elist ~loc exprs in
  [%expr Stdlib.String.concat ", " [%e exprs]]

and to_expression_string ~loc (expression : Ast.expression) =
  match expression with
  | Ast.Identifier _ -> failwith "Identifier"
  | Ast.String _ -> failwith "String"
  | Ast.BitString _ -> failwith "BitString"
  | Ast.Number _ -> failwith "Number"
  | Ast.TypeCast _ -> failwith "TypeCast"
  | Ast.PositionalParam _ -> failwith "PositionalParam"
  | Ast.ColumnReference (column_ref, field) ->
    of_column_reference ~loc (column_ref, field)
  | Ast.Index (_, _) -> failwith "Index"
  | Ast.BinaryExpression (_, _, _) -> failwith "BinaryExpression"
  | Ast.UnaryExpression (_, _) -> failwith "UnaryExpression"
  | Ast.FunctionCall (_, _) -> failwith "FunctionCall"
  | Ast.Null -> failwith "Null"

and of_column_reference ~loc (column_ref, field) =
  match column_ref, field with
  | Table (Module ident), Field (_, _, Unquoted field) ->
    let table = table_relation ~loc ident in
    let field = Ast_builder.Default.estring ~loc field in
    (* Ast_builder.Default.estring ~loc (Fmt.str "%s.%s" table field) *)
    [%expr Stdlib.Format.sprintf "%s.%s" [%e table] [%e field]]
  | _ -> failwith "column_ref"
;;
