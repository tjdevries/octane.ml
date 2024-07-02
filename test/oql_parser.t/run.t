Execute Test Suite:
  $ oql_parse ./examples/
  
  ===== ./examples/select_with_constants.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (Column { Ast.Column.schema = None; table = None; field = field }),
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
             (Column { Ast.Column.schema = None; table = None; field = name }),
             None))
           ]
         };
       from = (Some (From [(Model Users)])); where = None })
  
  ===== ./examples/from.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField { Ast.ModelField.model = User; field = id }), None))
           ]
         };
       from = (Some (From [(Model User)])); where = None })
  
  ===== ./examples/from_with_positional_param.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField { Ast.ModelField.model = User; field = id }), None))
           ]
         };
       from = (Some (From [(Model User)]));
       where =
       (Some (BinaryExpression (
                (ModelField { Ast.ModelField.model = User; field = id }), Eq,
                (PositionalParam 1))))
       })
  
  ===== ./examples/multi_select.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField { Ast.ModelField.model = User; field = id }), None));
           (Expression (
              (ModelField { Ast.ModelField.model = Post; field = author }),
              None))
           ]
         };
       from = (Some (From [(Model User); (Model Post); (Table tables)]));
       where = None })
  
  ===== ./examples/from_with_named_param.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField { Ast.ModelField.model = User; field = id }), None))
           ]
         };
       from = (Some (From [(Model User)]));
       where =
       (Some (BinaryExpression (
                (ModelField { Ast.ModelField.model = User; field = id }), Eq,
                (NamedParam "id"))))
       })
  
  ===== ./examples/simple_join.sql =====
  (Select
     { select =
       { result_kind = None;
         result_columns =
         [(Expression (
             (ModelField { Ast.ModelField.model = User; field = name }), None));
           (Expression (
              (ModelField { Ast.ModelField.model = Post; field = content }),
              None))
           ]
         };
       from =
       (Some (Join
                { relation = (Model Post);
                  stanzas =
                  [(Inner, (Model User),
                    (On
                       (BinaryExpression (
                          (ModelField
                             { Ast.ModelField.model = User; field = id }),
                          Eq,
                          (ModelField
                             { Ast.ModelField.model = Post; field = author })
                          ))))
                    ]
                  }));
       where = None })
  
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
              (FunctionCall (sqrt,
                 [(BinaryExpression ((NumericLiteral (Integer 2)), Add,
                     (NumericLiteral (Integer 3))))
                   ]
                 )),
              None));
           (Expression ((StringLiteral (SingleQuote "end")), None))]
         };
       from = None; where = None })
