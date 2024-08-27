open Core

let get_valid_models ast =
  let open Ast in
  match ast with
  | Select { from = Some from; _ } ->
    FromClause.relations from
    |> List.filter_map ~f:(function
      | Model m -> Some m
      | _ -> None)
    |> List.dedup_and_sort ~compare:Model.compare
  | _ -> []
;;

let get_models_from_expression expr =
  let open Ast in
  let rec search expr acc =
    match expr with
    | ModelField m -> m :: acc
    | BinaryExpression (left, _, right) -> acc |> search left |> search right
    | UnaryExpression (_, expr) -> search expr acc
    | _ -> acc
  in
  search expr []
;;

let get_used_models ast =
  let open Ast in
  match ast with
  | Select { select = { result_columns; _ }; _ } ->
    List.fold_left result_columns ~init:[] ~f:(fun acc ->
        function
        | Expression (expr, _) -> get_models_from_expression expr @ acc
        | _ -> acc)
    |> List.dedup_and_sort ~compare:ModelField.compare
;;

let get_invalid_model ast =
  let used_models = get_used_models ast in
  let valid_models = get_valid_models ast in
  List.find used_models ~f:(fun m ->
    not (List.mem valid_models m.model ~equal:Ast.Model.equal))
;;

type params =
  { named : string list
  ; positional : int list
  }
[@@deriving show]

let find_params (ast : Ast.t) =
  let open Ast in
  let rec search expr acc =
    match expr with
    | PositionalParam pos ->
      if List.mem acc.positional pos ~equal:Int.equal
      then acc
      else { acc with positional = pos :: acc.positional }
    | NamedParam named ->
      if List.mem acc.named named ~equal:String.equal
      then acc
      else { acc with named = named :: acc.named }
    | BinaryExpression (left, _, right) -> acc |> search left |> search right
    | UnaryExpression (_, expr) -> search expr acc
    | FunctionCall (_, _) -> failwith "function call"
    | StringLiteral _ | BitString _ | TypeCast _ | Index (_, _) | Null | _ ->
      acc
  in
  match ast with
  | Select { select = { result_columns; _ }; from = Some _; where } ->
    let expressions =
      List.filter_map result_columns ~f:(function
        | Expression (expr, _) -> Some expr
        | _ -> None)
    in
    let acc = { named = []; positional = [] } in
    let acc =
      List.fold_left ~init:acc ~f:(fun acc expr -> search expr acc) expressions
    in
    let acc =
      Option.fold ~init:acc ~f:(fun acc where -> search where acc) where
    in
    { acc with positional = List.sort acc.positional ~compare:Int.compare }
  | _ -> assert false
;;

let get_type_of_expression expr =
  let open Ast in
  match expr with
  | ModelField _ as expr -> Some expr
  (* | ColumnReference (Table (Module m), f) as e -> Some e *)
  | _ -> None
;;

let get_type_of_named_param ast param =
  let open Ast in
  let rec search expr =
    match expr with
    | BinaryExpression (left, _op, right) when Ast.equal_expression right param
      -> get_type_of_expression left
    | BinaryExpression (left, _op, _right) when Ast.equal_expression left param
      -> failwith "param matches left"
    | BinaryExpression (_left, _op, _right) -> None
    | UnaryExpression (_, expr) -> search expr
    | FunctionCall (_, _) -> failwith "function call"
    | _ -> None
  in
  match ast with
  | Select { where = Some where; _ } -> search where
  | _ -> None
;;

let print_params str =
  let ast = Run.parse str in
  let ast = Result.ok_or_failwith ast in
  let params = find_params ast in
  Fmt.pr "%a\n" pp_params params
;;

let%expect_test "positional params" =
  print_params "SELECT User.id FROM User WHERE User.id = $id";
  [%expect {| { Analysis.named = ["id"]; positional = [] } |}]
;;

let%expect_test "multiple positional params" =
  print_params "SELECT User.id, $1, $2 FROM User";
  [%expect {| { Analysis.named = []; positional = [1; 2] } |}]
;;

let%expect_test "named params" =
  print_params "SELECT User.id FROM User WHERE User.id = $1";
  [%expect {| { Analysis.named = []; positional = [1] } |}]
;;

let%expect_test "duplicate named params" =
  print_params "SELECT User.id FROM User WHERE $1 = $1";
  [%expect {| { Analysis.named = []; positional = [1] } |}]
;;
