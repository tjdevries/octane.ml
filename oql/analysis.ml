open Base

type rule = string

(* let find_tables ast = *)
(*   let open Ast in *)
(*   match ast with *)
(*   | Select { relation; _ } -> *)
(*     Option.map relation ~f:(fun relation -> assert false) *)
(* ;; *)

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
  (* | ColumnReference (Table (Module m), f) as e -> Some e *)
  | _ -> None
;;

let get_type_of_named_param ast param =
  let open Ast in
  let rec search expr =
    match expr with
    | BinaryExpression (left, op, right) when Ast.equal_expression right param
      -> get_type_of_expression left
    | BinaryExpression (left, op, right) when Ast.equal_expression left param ->
      failwith "param matches left"
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
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "At offset 7: syntax error.\n")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__Result.ok_or_failwith in file "src/result.ml" (inlined), line 256, characters 17-29
  Called from Oql__Analysis.print_params in file "oql/analysis.ml", line 81, characters 12-37
  Called from Oql__Analysis.(fun) in file "oql/analysis.ml", line 87, characters 2-61
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "multiple positional params" =
  print_params "SELECT User.id, $1, $2 FROM User";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "At offset 7: syntax error.\n")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__Result.ok_or_failwith in file "src/result.ml" (inlined), line 256, characters 17-29
  Called from Oql__Analysis.print_params in file "oql/analysis.ml", line 81, characters 12-37
  Called from Oql__Analysis.(fun) in file "oql/analysis.ml", line 104, characters 2-49
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "named params" =
  print_params "SELECT User.id FROM User WHERE User.id = $1";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "At offset 7: syntax error.\n")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__Result.ok_or_failwith in file "src/result.ml" (inlined), line 256, characters 17-29
  Called from Oql__Analysis.print_params in file "oql/analysis.ml", line 81, characters 12-37
  Called from Oql__Analysis.(fun) in file "oql/analysis.ml", line 121, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "duplicate named params" =
  print_params "SELECT User.id FROM User WHERE $1 = $1";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "At offset 7: syntax error.\n")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__Result.ok_or_failwith in file "src/result.ml" (inlined), line 256, characters 17-29
  Called from Oql__Analysis.print_params in file "oql/analysis.ml", line 81, characters 12-37
  Called from Oql__Analysis.(fun) in file "oql/analysis.ml", line 138, characters 2-55
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
