type param =
  | Positional of int
  | Named of string

let find_params (expr : Oql.Ast.expression) =
  let rec search expr acc =
    match expr with
    | Oql.Ast.PositionalParam pos -> Positional pos :: acc
    | Oql.Ast.NamedParam str -> Named str :: acc
    | Oql.Ast.ColumnReference (_, _) -> failwith "column reference"
    | Oql.Ast.BinaryExpression (left, _, right) ->
      acc |> search left |> search right
    | Oql.Ast.UnaryExpression (_, expr) -> search expr acc
    | Oql.Ast.TypeCast _ -> failwith "type cast"
    | Oql.Ast.FunctionCall (_, _) -> failwith "function call"
    | Oql.Ast.Identifier _
    | Oql.Ast.String _
    | Oql.Ast.BitString _
    | Oql.Ast.Number _
    | Oql.Ast.Index (_, _)
    | Oql.Ast.Null -> acc
  in
  search expr []
;;
