type param =
  | Positional of int
  | Named of string

let find_params (expr : Oql.Ast.expression) =
  let rec search expr acc =
    match expr with
    | Oql.Ast.PositionalParam pos -> Positional pos :: acc
    | Oql.Ast.NamedParam str -> Named str :: acc
    | Oql.Ast.Column _ -> failwith "column reference"
    | Oql.Ast.BinaryExpression (left, _, right) ->
      acc |> search left |> search right
    | Oql.Ast.UnaryExpression (_, expr) -> search expr acc
    | Oql.Ast.TypeCast _ -> failwith "type cast"
    | Oql.Ast.FunctionCall (_, _) -> failwith "function call"
    | Oql.Ast.StringLiteral _ | Oql.Ast.BitString _ | Oql.Ast.NumericLiteral _
    | Oql.Ast.Index (_, _)
    | Oql.Ast.Null | _ -> acc
  in
  search expr []
;;
