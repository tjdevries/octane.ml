Execute Test Suite:
  $ oql_parse ./examples/
  
  ===== ./examples/select_with_constants.sql =====
  (Select
     { expressions =
       [(Identifier (Unquoted "unquoted_field"));
         (Identifier (Unquoted "my_field")); (String (SingleQuote "string"));
         (BitString "B'010101'"); (Number (Integer 5)); (Number (Numeric 5.));
         (Number (Numeric 5.)); (Number (Integer 5000));
         (TypeCast ((Unquoted "bool"), (SingleQuote "yes")));
         (TypeCast ((Unquoted "bool"), (SingleQuote "yes")));
         (TypeCast ((Unquoted "bool"), (SingleQuote "yes")));
         (PositionalParam 1); (PositionalParam 37); (Number (Integer 1));
         (String (SingleQuote "a string"));
         (ColumnReference ((Table (Unquoted "tbl")),
            (Field (<1:237>, <1:240>, (Unquoted "col")))));
         (ColumnReference ((Table (Unquoted "mytbl")),
            (Field (<1:250>, <1:255>, (Unquoted "other")))));
         (Index ((Identifier (Unquoted "arr")), (Specific (Number (Integer 0)))
            ));
         (Index ((Identifier (Unquoted "arr")),
            (Slice ((Identifier (Unquoted "start")),
               (Identifier (Unquoted "stop"))))
            ));
         (Index (
            (Index (
               (ColumnReference ((Table (Unquoted "mytable")),
                  (Field (<1:303>, <1:315>, (Unquoted "two_d_column"))))),
               (Specific (Number (Integer 17))))),
            (Specific (Number (Integer 34)))));
         (ColumnReference ((Table (Unquoted "tbl")),
            (Field (<1:352>, <1:356>, (Unquoted "col2")))));
         (ColumnReference ((Table (Unquoted "tbl")), Star));
         (String (SingleQuote "the end"))];
       relation = None; where = None })
  
  ===== ./examples/simple_select.sql =====
  (Select
     { expressions = [(Identifier (Unquoted "name"))];
       relation = (Some "Users"); where = None })
  
  ===== ./examples/from.sql =====
  (Select
     { expressions =
       [(ColumnReference ((Table (Module "User")),
           (Field (<1:12>, <1:14>, (Unquoted "id")))))
         ];
       relation = (Some "User");
       where =
       (Some (BinaryExpression (
                (ColumnReference ((Table (Module "User")),
                   (Field (<1:36>, <1:38>, (Unquoted "id"))))),
                Eq, (Number (Integer 1)))))
       })
  
  ===== ./examples/from_with_positional_param.sql =====
  (Select
     { expressions =
       [(ColumnReference ((Table (Module "User")),
           (Field (<1:12>, <1:14>, (Unquoted "id")))))
         ];
       relation = (Some "User");
       where =
       (Some (BinaryExpression (
                (ColumnReference ((Table (Module "User")),
                   (Field (<1:36>, <1:38>, (Unquoted "id"))))),
                Eq, (PositionalParam 1))))
       })
  
  ===== ./examples/from_with_named_param.sql =====
  (Select
     { expressions =
       [(ColumnReference ((Table (Module "User")),
           (Field (<1:12>, <1:14>, (Unquoted "id")))))
         ];
       relation = (Some "User");
       where =
       (Some (BinaryExpression (
                (ColumnReference ((Table (Module "User")),
                   (Field (<1:36>, <1:38>, (Unquoted "id"))))),
                Eq, (NamedParam "id"))))
       })
  
  ===== ./examples/operators.sql =====
  (Select
     { expressions =
       [(BinaryExpression ((Number (Integer 1)), Add, (Number (Integer 2))));
         (BinaryExpression ((Number (Integer 1)), Add,
            (BinaryExpression ((Number (Integer 2)), Mul, (Number (Integer 3))
               ))
            ));
         (UnaryExpression (Neg, (Number (Integer 1))));
         (FunctionCall ((Unquoted "sqrt"),
            [(BinaryExpression ((Number (Integer 2)), Add, (Number (Integer 3))
                ))
              ]
            ));
         (String (SingleQuote "end"))];
       relation = None; where = None })
