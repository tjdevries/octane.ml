Execute Test Suite:
  $ oql_parse ./examples/
  
  ===== ./examples/select_with_constants.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (Column
                { Ast.Column.schema = None; table = None;
                  field = (<1:7>, <1:12>, "field") }),
             None))
           ]
         };
       from = None; where = None })
  
  ===== ./examples/simple_select.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (Column
                { Ast.Column.schema = None; table = None;
                  field = (<1:7>, <1:11>, "name") }),
             None))
           ]
         };
       from =
       (Some { relation = [(Model (<1:17>, <1:22>, "Users"))]; join = None });
       where = None })
  
  ===== ./examples/from.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField
                { Ast.ModelField.model = (<1:7>, <1:11>, "User");
                  field = (<1:12>, <1:14>, "id") }),
             None))
           ]
         };
       from =
       (Some { relation = [(Model (<1:20>, <1:24>, "User"))]; join = None });
       where = None })
  
  ===== ./examples/from_with_positional_param.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField
                { Ast.ModelField.model = (<1:7>, <1:11>, "User");
                  field = (<1:12>, <1:14>, "id") }),
             None))
           ]
         };
       from =
       (Some { relation = [(Model (<1:20>, <1:24>, "User"))]; join = None });
       where =
       (Some (BinaryExpression (
                (ModelField
                   { Ast.ModelField.model = (<1:31>, <1:35>, "User");
                     field = (<1:36>, <1:38>, "id") }),
                Eq, (PositionalParam 1))))
       })
  
  ===== ./examples/from_with_named_param.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField
                { Ast.ModelField.model = (<1:7>, <1:11>, "User");
                  field = (<1:12>, <1:14>, "id") }),
             None))
           ]
         };
       from =
       (Some { relation = [(Model (<1:20>, <1:24>, "User"))]; join = None });
       where =
       (Some (BinaryExpression (
                (ModelField
                   { Ast.ModelField.model = (<1:31>, <1:35>, "User");
                     field = (<1:36>, <1:38>, "id") }),
                Eq, (NamedParam "id"))))
       })
  
  ===== ./examples/operators.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (BinaryExpression ((NumericLiteral (Integer 1)), Add,
                (NumericLiteral (Integer 2)))),
             None));
           (Expression (
              (BinaryExpression ((NumericLiteral (Integer 1)), Add,
                 (BinaryExpression ((NumericLiteral (Integer 2)), Mul,
                    (NumericLiteral (Integer 3))))
                 )),
              None));
           (Expression ((UnaryExpression (Neg, (NumericLiteral (Integer 1)))),
              None));
           (Expression (
              (FunctionCall ((<1:29>, <1:33>, "sqrt"),
                 [(BinaryExpression ((NumericLiteral (Integer 2)), Add,
                     (NumericLiteral (Integer 3))))
                   ]
                 )),
              None));
           (Expression ((StringLiteral (SingleQuote "end")), None))]
         };
       from = None; where = None })
