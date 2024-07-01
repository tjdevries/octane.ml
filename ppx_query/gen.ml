open Ppxlib
open Oql
open Base

let make_positional_param_expr ~loc i =
  let ident = Loc.make ~loc (Lident ("p" ^ Int.to_string i)) in
  Ast_builder.Default.pexp_ident ~loc ident
;;

let table_relation ~loc relation =
  let relation = Ast.get_relation_ident relation in
  let ident = Ldot (Lident relation, "relation") in
  let ident = Loc.make ~loc ident in
  Ast_helper.Exp.ident ~loc ident
;;

let module_param ~loc module_name param_name =
  (* module_name.Params.param_name *)
  let ident = Ldot (Lident module_name, "Params") in
  let ident = Ldot (ident, param_name) in
  let ident = Loc.make ~loc ident in
  Ast_helper.Exp.ident ~loc ident
;;

let get_param ~loc correlation field =
  match correlation with
  | Ast.Table t -> Ast.Table.name t (* User.Fields.param_id id *)
  | _ -> failwith "get_param: correlation"
;;

let make_fun ~loc arg body =
  let arg = Loc.make ~loc arg in
  let pattern = Ast_builder.Default.ppat_var ~loc arg in
  Ast_builder.Default.pexp_fun ~loc Nolabel None pattern body
;;

let make_labelled_fun ~loc arg body =
  let arg_loc = Loc.make ~loc arg in
  let pattern = Ast_builder.Default.ppat_var ~loc arg_loc in
  Ast_builder.Default.pexp_fun ~loc (Labelled arg) None pattern body
;;

let type_of_expression_to_generated_expression ~loc type_of_expr expr =
  let expr = Ast_builder.Default.evar ~loc expr in
  let open Ast in
  match type_of_expr with
  | Some (ModelField model_field) ->
    let model = ModelField.model_name model_field in
    let field = ModelField.field_name model_field in
    let param_fn = module_param ~loc model field in
    [%expr [%e param_fn] [%e expr]]
  | Some (Column c) -> failwith "Need to implement Column support"
  | _ -> expr
;;

let rec of_ast ~loc (ast : Ast.t) =
  let _ = ast in
  let params = Analysis.find_params ast in
  match ast with
  | Select select ->
    let query_expr = to_select_string ~loc select in
    let paramlist =
      List.fold_left params.positional ~init:[] ~f:(fun acc pos ->
        Fmt.str "p%d" pos :: acc)
    in
    let paramlist = List.rev_append paramlist params.named in
    let paramslist =
      List.map paramlist ~f:(fun param ->
        let type_constraint =
          Analysis.get_type_of_named_param ast (NamedParam param)
        in
        type_of_expression_to_generated_expression ~loc type_constraint param)
    in
    let params_expr = Ast_builder.Default.elist ~loc paramslist in
    (* let f = Ast_helper.Exp.fun_ in *)
    (* let arg_label *)
    let body =
      [%expr
        let query = [%e query_expr] in
        let params = [%e params_expr] in
        Fmt.epr "query: %s@." query;
        Silo_postgres.query db ~query ~params ~deserializer:deserialize]
    in
    let body =
      List.fold_right params.positional ~init:body ~f:(fun pos body ->
        make_fun ~loc (Fmt.str "p%d" pos) body)
    in
    let body =
      List.fold params.named ~init:body ~f:(fun body pos ->
        make_labelled_fun ~loc pos body)
    in
    let f = make_fun ~loc "db" body in
    [%stri let query = [%e f]]

and to_select_string ~loc (select : Ast.select_statement) =
  match select.from with
  | Some { relation = [ relation ]; _ } ->
    let table = table_relation ~loc relation in
    let expressions = Ast.get_select_expressions select.select in
    let selection = of_expressions ~loc expressions in
    let e =
      match select.where with
      | Some w ->
        let where = of_expression ~loc w in
        [%expr
          Stdlib.Format.sprintf
            "SELECT %s FROM %s WHERE %s"
            [%e selection]
            [%e table]
            [%e where]]
      | None ->
        [%expr
          Stdlib.Format.sprintf "SELECT %s FROM %s" [%e selection] [%e table]]
    in
    e
  | Some _ -> failwith "unsupported froom clause"
  | None -> failwith "no relations: must be arch user"

and of_expressions ~loc (expressions : Ast.expression list) =
  let exprs = List.map ~f:(of_expression ~loc) expressions in
  let exprs = Ast_builder.Default.elist ~loc exprs in
  [%expr Stdlib.String.concat ", " [%e exprs]]

and of_expression ~loc (expression : Ast.expression) =
  match expression with
  | Ast.NumericLiteral _ -> failwith "Number"
  | Ast.StringLiteral _ -> failwith "String"
  | Ast.BitString _ -> failwith "BitString"
  | Ast.TypeCast _ -> failwith "TypeCast"
  | Ast.PositionalParam pos -> make_positional_param_expr ~loc pos
  | Ast.NamedParam name -> [%expr "KEKW"]
  | Ast.Column col -> of_column ~loc col
  | Ast.Index (_, _) -> failwith "Index"
  | Ast.BinaryExpression (left, op, right) ->
    of_binary_expression ~loc left op right
  | Ast.UnaryExpression (_, _) -> failwith "UnaryExpression"
  | Ast.FunctionCall (_, _) -> failwith "FunctionCall"
  | Ast.Null -> failwith "Null"
  | Ast.ModelField m -> of_model_field ~loc m
  (* | Ast.ColumnReference (column_ref, field) -> *)
  (*   of_column_reference ~loc (column_ref, field) *)
  | _ -> failwith "unsupported expression"

and of_binary_expression ~loc left op right =
  let open Oql.Ast in
  (* User.id = $id *)
  (* left = User.id, Equal, right = $id *)
  (* let left = of_expression ~loc left in *)
  (* let right = of_expression ~loc right in *)
  (* let op = of_bitop ~loc op in *)
  (* [%expr Stdlib.Format.sprintf "(%s %s %s)" [%e left] [%e op] [%e right]] *)
  match left, right with
  (*   let _ = get_param ~loc correlation field in *)
  | ModelField model_field, NamedParam param -> [%expr "TODO"]
  | ModelField model_field, PositionalParam pos -> [%expr "TODO"]
  | _ -> failwith "binary expression: not supported"

and of_bitop ~loc op =
  match op with
  | Ast.Add -> Ast_builder.Default.estring ~loc "+"
  | Ast.Eq -> Ast_builder.Default.estring ~loc "="
  | _ -> failwith "bitop"

and of_model_field ~loc m =
  let open Ast in
  let ident = Ldot (Lident (ModelField.model_name m), "relation") in
  let ident = Loc.make ~loc ident in
  let table = Ast_helper.Exp.ident ~loc ident in
  let field = Ast_builder.Default.estring ~loc (ModelField.field_name m) in
  [%expr Stdlib.Format.sprintf "%s.%s" [%e table] [%e field]]

and of_column ~loc col =
  match col with
  (* | Table (Module ident), Field (_, _, Unquoted field) -> *)
  (*   (* TODO: This is not how we want table refs to work *) *)
  (*   let table = table_relation ~loc (Table ident) in *)
  (*   let field = Ast_builder.Default.estring ~loc field in *)
  (*   (* Ast_builder.Default.estring ~loc (Fmt.str "%s.%s" table field) *) *)
  (*   [%expr Stdlib.Format.sprintf "%s.%s" [%e table] [%e field]] *)
  | _ -> failwith "column_ref"
;;
