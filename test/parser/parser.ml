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
        LetStatement { identifier = Identifier { value = "x" }; value = Empty };
        LetStatement { identifier = Identifier { value = "y" }; value = Empty };
        LetStatement
          { identifier = Identifier { value = "foobar" }; value = Empty };
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
    Ast.[ExpressionStatement { expression = Identifier { value = "foobar" } }]
    (Lexer.make code |> To_test.ast)

let test_integer_literal_expression () =
  let code = "5;" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.[ExpressionStatement { expression = IntegerLiteral { value = 5 } }]
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
        ] );
    ]
