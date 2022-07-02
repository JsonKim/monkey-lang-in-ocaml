open Monkey

module To_test = struct
  let ast lex = Parser.parse_program (Parser.make lex)
end

let ast_testable = Alcotest.testable Ast.pp_statement Ast.equal_statement

let test_let_statements () =
  let code = "\nlet x = 5;\nlet y = 10;\nlet foobar = 8383838;\n" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.
      [
        LetStatement { identifier = "x"; value = Empty };
        LetStatement { identifier = "y"; value = Empty };
        LetStatement { identifier = "foobar"; value = Empty };
      ]
    (Lexer.make code |> To_test.ast)

let test_return_statements () =
  let code = "\nreturn 5;\nreturn 10;\nreturn 9933222;\n" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.
      [
        ReturnStatement { value = Empty };
        ReturnStatement { value = Empty };
        ReturnStatement { value = Empty };
      ]
    (Lexer.make code |> To_test.ast)

let test_identifier_expression () =
  let code = "foobar;" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.[ExpressionStatement { expression = Identifier "foobar" }]
    (Lexer.make code |> To_test.ast)

let test_integer_literal_expression () =
  let code = "5;" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.[ExpressionStatement { expression = int_to_literal 5 }]
    (Lexer.make code |> To_test.ast)

let test_boolean_literal_expression () =
  let code = "true;\nfalse\n" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.
      [
        ExpressionStatement { expression = bool_to_literal true };
        ExpressionStatement { expression = bool_to_literal false };
      ]
    (Lexer.make code |> To_test.ast)

let test_prefix_expression () =
  let code = "!5;\n-15;" in
  let open Ast in
  let open Token in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        { expression = Prefix { token = Bang; right = int_to_literal 5 } };
      ExpressionStatement
        { expression = Prefix { token = Minus; right = int_to_literal 15 } };
    ]
    (Lexer.make code |> To_test.ast)

let test_infix_expression () =
  let code = "1 + 2 + 3;\n-a * b\nx + y * z" in
  let open Ast in
  let open Token in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Plus;
                left =
                  Infix
                    {
                      token = Plus;
                      left = int_to_literal 1;
                      right = int_to_literal 2;
                    };
                right = int_to_literal 3;
              };
        };
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Asterisk;
                left = Prefix { token = Minus; right = Identifier "a" };
                right = Identifier "b";
              };
        };
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Plus;
                left = Identifier "x";
                right =
                  Infix
                    {
                      token = Asterisk;
                      left = Identifier "y";
                      right = Identifier "z";
                    };
              };
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_grouped_expression () =
  let code = "(x + y) * z;\n(1 + 2 * 3) * 4;\n-(a - b);\n-a - b" in
  let open Ast in
  let open Token in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Asterisk;
                left =
                  Infix
                    {
                      token = Plus;
                      left = Identifier "x";
                      right = Identifier "y";
                    };
                right = Identifier "z";
              };
        };
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Asterisk;
                left =
                  Infix
                    {
                      token = Plus;
                      left = int_to_literal 1;
                      right =
                        Infix
                          {
                            token = Asterisk;
                            left = int_to_literal 2;
                            right = int_to_literal 3;
                          };
                    };
                right = int_to_literal 4;
              };
        };
      ExpressionStatement
        {
          expression =
            Prefix
              {
                token = Minus;
                right =
                  Infix
                    {
                      token = Minus;
                      left = Identifier "a";
                      right = Identifier "b";
                    };
              };
        };
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Minus;
                left = Prefix { token = Minus; right = Identifier "a" };
                right = Identifier "b";
              };
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_if_expression () =
  let code = "if (x < y) { 1+2; x }" in
  let open Ast in
  let open Token in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        {
          expression =
            If
              {
                condition =
                  Infix
                    {
                      token = LT;
                      left = Identifier "x";
                      right = Identifier "y";
                    };
                consequence =
                  [
                    ExpressionStatement
                      {
                        expression =
                          Infix
                            {
                              token = Plus;
                              left = int_to_literal 1;
                              right = int_to_literal 2;
                            };
                      };
                    ExpressionStatement { expression = Identifier "x" };
                  ];
                alternative = None;
              };
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_if_else_expression () =
  let code = "if (x < y) { x } else { y }" in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      Ast.ExpressionStatement
        {
          expression =
            Ast.If
              {
                condition =
                  Ast.Infix
                    {
                      token = Token.LT;
                      left = Ast.Identifier "x";
                      right = Ast.Identifier "y";
                    };
                consequence =
                  [Ast.ExpressionStatement { expression = Ast.Identifier "x" }];
                alternative =
                  Some
                    [
                      Ast.ExpressionStatement { expression = Ast.Identifier "y" };
                    ];
              };
        };
    ]
    (Lexer.make code |> To_test.ast)

let () =
  let open Alcotest in
  test_let_statements |> ignore;
  test_return_statements |> ignore;
  run "Parser"
    [
      ( "parser test",
        [
          (* test_case "parse LetStatement" `Slow test_let_statements;
             test_case "parse ReturnStatement" `Slow test_return_statements; *)
          test_case "parse IdentifireExpression" `Slow
            test_identifier_expression;
          test_case "parse IntegerLiteralExpression" `Slow
            test_integer_literal_expression;
          test_case "parse BooleanLiteralExpression" `Slow
            test_boolean_literal_expression;
          test_case "parse PrefixExpression" `Slow test_prefix_expression;
          test_case "parse InfixExpression" `Slow test_infix_expression;
          test_case "parse grouped Expression" `Slow test_grouped_expression;
          test_case "parse if Expression" `Slow test_if_expression;
          test_case "parse if-else Expression" `Slow test_if_else_expression;
        ] );
    ]