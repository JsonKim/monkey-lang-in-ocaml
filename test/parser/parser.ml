open Monkey

exception Not_Program

module To_test = struct
  let ast lex =
    match lex |> Parser.make |> Parser.parse_program |> snd with
    | Ast.Program program -> program
    | _ -> raise Not_Program
end

let ast_testable = Alcotest.testable Ast.pp_statement Ast.equal_statement
let ast_node_testable = Alcotest.testable Ast.pp_node Ast.equal_node

let test_let_statements () =
  let code =
    "\n\
     let x = 5;\n\
     let y = 10;\n\
     let foobar = x + y;\n\
     let f = fn(a, b) { a < b; }" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.
      [
        LetStatement { identifier = "x"; value = Literal (Integer 5) };
        LetStatement { identifier = "y"; value = Literal (Integer 10) };
        LetStatement
          {
            identifier = "foobar";
            value =
              Infix
                {
                  token = Token.Plus;
                  left = Identifier "x";
                  right = Identifier "y";
                };
          };
        LetStatement
          {
            identifier = "f";
            value =
              Function
                {
                  parameters = ["a"; "b"];
                  body =
                    [
                      ExpressionStatement
                        {
                          expression =
                            Infix
                              {
                                token = Token.LT;
                                left = Identifier "a";
                                right = Identifier "b";
                              };
                        };
                    ];
                };
          };
      ]
    (Lexer.make code |> To_test.ast)

let test_return_statements () =
  let code = "return 5;\nreturn x + y;\nreturn fn(x, y) { return x + y; }" in
  Alcotest.(check (list ast_testable))
    "same ast"
    Ast.
      [
        ReturnStatement { value = Literal (Integer 5) };
        ReturnStatement
          {
            value =
              Infix
                {
                  token = Token.Plus;
                  left = Identifier "x";
                  right = Identifier "y";
                };
          };
        ReturnStatement
          {
            value =
              Function
                {
                  parameters = ["x"; "y"];
                  body =
                    [
                      ReturnStatement
                        {
                          value =
                            Infix
                              {
                                token = Token.Plus;
                                left = Identifier "x";
                                right = Identifier "y";
                              };
                        };
                    ];
                };
          };
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

let test_string_literal_expression () =
  let code = "\"hello world\";" in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      Ast.ExpressionStatement
        { expression = Ast.Literal (Ast.String "hello world") };
    ]
    (Lexer.make code |> To_test.ast)

let test_array_literal_expression () =
  let code = "[1, 2 + 2, 3 * 4, \"test\"]" in
  let open Ast in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        {
          expression =
            Literal
              (Array
                 [
                   Literal (Integer 1);
                   Infix
                     {
                       token = Token.Plus;
                       left = Literal (Integer 2);
                       right = Literal (Integer 2);
                     };
                   Infix
                     {
                       token = Token.Asterisk;
                       left = Literal (Integer 3);
                       right = Literal (Integer 4);
                     };
                   Literal (String "test");
                 ]);
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_hash_literal_expression () =
  let code =
    "{};\n\
     {\"one\":1, \"two\":2, \"three\": 15 / 5, 1 < 2: \"1 < 2\"};\n\
     [{\"name\":\"Alice\", \"age\":24},{\"name\":\"Anna\",\"age\": 28}]" in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      Ast.ExpressionStatement { expression = Ast.Literal (Ast.Hash []) };
      Ast.ExpressionStatement
        {
          expression =
            Ast.Literal
              (Ast.Hash
                 [
                   ( Ast.Infix
                       {
                         token = Token.LT;
                         left = Ast.Literal (Ast.Integer 1);
                         right = Ast.Literal (Ast.Integer 2);
                       },
                     Ast.Literal (Ast.String "1 < 2") );
                   ( Ast.Literal (Ast.String "three"),
                     Ast.Infix
                       {
                         token = Token.Slash;
                         left = Ast.Literal (Ast.Integer 15);
                         right = Ast.Literal (Ast.Integer 5);
                       } );
                   (Ast.Literal (Ast.String "two"), Ast.Literal (Ast.Integer 2));
                   (Ast.Literal (Ast.String "one"), Ast.Literal (Ast.Integer 1));
                 ]);
        };
      Ast.ExpressionStatement
        {
          expression =
            Ast.Literal
              (Ast.Array
                 [
                   Ast.Literal
                     (Ast.Hash
                        [
                          ( Ast.Literal (Ast.String "age"),
                            Ast.Literal (Ast.Integer 24) );
                          ( Ast.Literal (Ast.String "name"),
                            Ast.Literal (Ast.String "Alice") );
                        ]);
                   Ast.Literal
                     (Ast.Hash
                        [
                          ( Ast.Literal (Ast.String "age"),
                            Ast.Literal (Ast.Integer 28) );
                          ( Ast.Literal (Ast.String "name"),
                            Ast.Literal (Ast.String "Anna") );
                        ]);
                 ]);
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_array_index_expression () =
  let code =
    "myArray[1 + 1]\n\
     a * [1, 2, 3, 4][b * c] * d\n\
     add(a * b[2], b[1], 2 * [1, 2][1])" in
  let open Ast in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        {
          expression =
            Index
              {
                left = Identifier "myArray";
                index =
                  Infix
                    {
                      token = Token.Plus;
                      left = Literal (Integer 1);
                      right = Literal (Integer 1);
                    };
              };
        };
      ExpressionStatement
        {
          expression =
            Infix
              {
                token = Token.Asterisk;
                left =
                  Infix
                    {
                      token = Token.Asterisk;
                      left = Identifier "a";
                      right =
                        Index
                          {
                            left =
                              Literal
                                (Array
                                   [
                                     Literal (Integer 1);
                                     Literal (Integer 2);
                                     Literal (Integer 3);
                                     Literal (Integer 4);
                                   ]);
                            index =
                              Infix
                                {
                                  token = Token.Asterisk;
                                  left = Identifier "b";
                                  right = Identifier "c";
                                };
                          };
                    };
                right = Identifier "d";
              };
        };
      ExpressionStatement
        {
          expression =
            Call
              {
                fn = Identifier "add";
                arguments =
                  [
                    Infix
                      {
                        token = Token.Asterisk;
                        left = Identifier "a";
                        right =
                          Index
                            {
                              left = Identifier "b";
                              index = Literal (Integer 2);
                            };
                      };
                    Index { left = Identifier "b"; index = Literal (Integer 1) };
                    Infix
                      {
                        token = Token.Asterisk;
                        left = Literal (Integer 2);
                        right =
                          Index
                            {
                              left =
                                Literal
                                  (Array
                                     [Literal (Integer 1); Literal (Integer 2)]);
                              index = Literal (Integer 1);
                            };
                      };
                  ];
              };
        };
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

let test_function_literal_expression () =
  let code = "fn () { 42 }\nfn(x, y) { x + y }" in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      Ast.ExpressionStatement
        {
          expression =
            Ast.Function
              {
                parameters = [];
                body =
                  [
                    Ast.ExpressionStatement
                      { expression = Ast.Literal (Ast.Integer 42) };
                  ];
              };
        };
      Ast.ExpressionStatement
        {
          expression =
            Ast.Function
              {
                parameters = ["x"; "y"];
                body =
                  [
                    Ast.ExpressionStatement
                      {
                        expression =
                          Ast.Infix
                            {
                              token = Token.Plus;
                              left = Ast.Identifier "x";
                              right = Ast.Identifier "y";
                            };
                      };
                  ];
              };
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_call_expression () =
  let code = "f();\ng(y);\nadd(2, 3);\nh(1 + 2)" in
  let open Ast in
  Alcotest.(check (list ast_testable))
    "same ast"
    [
      ExpressionStatement
        { expression = Call { fn = Identifier "f"; arguments = [] } };
      ExpressionStatement
        {
          expression =
            Call { fn = Identifier "g"; arguments = [Identifier "y"] };
        };
      ExpressionStatement
        {
          expression =
            Call
              {
                fn = Identifier "add";
                arguments = [Literal (Integer 2); Literal (Integer 3)];
              };
        };
      ExpressionStatement
        {
          expression =
            Call
              {
                fn = Identifier "h";
                arguments =
                  [
                    Infix
                      {
                        token = Token.Plus;
                        left = Literal (Integer 1);
                        right = Literal (Integer 2);
                      };
                  ];
              };
        };
    ]
    (Lexer.make code |> To_test.ast)

let test_modify () =
  let open Ast in
  let parse_code code =
    code
    |> Lexer.make
    |> Parser.make
    |> Parser.parse_expression_statement
    |> snd
    |> Option.get in
  let parse code = Statement (code |> parse_code) in
  let turnOneIntoTwo = function
    | Expression (Literal (Integer 1)) -> Expression (2 |> int_to_literal)
    | node -> node in
  let ast =
    [
      parse "2";
      parse "2 + 2";
      parse "2 + 2";
      parse "-2";
      parse "2[2]";
      parse "if (2) { let x = 2; } else { return 2 }";
      parse "fn (x) { x + 2 }";
      parse "[2, 2]";
      parse "{ 2: 2, 2: 2 }";
    ] in
  Alcotest.(check (list ast_node_testable))
    "same ast"
    [
      Modify.modify turnOneIntoTwo (parse "1");
      Modify.modify turnOneIntoTwo (parse "1 + 2");
      Modify.modify turnOneIntoTwo (parse "2 + 1");
      Modify.modify turnOneIntoTwo (parse "-1");
      Modify.modify turnOneIntoTwo (parse "1[1]");
      Modify.modify turnOneIntoTwo
        (parse "if (1) { let x = 1; } else { return 1 }");
      Modify.modify turnOneIntoTwo (parse "fn (x) { x + 1 }");
      Modify.modify turnOneIntoTwo (parse "[1, 1]");
      Modify.modify turnOneIntoTwo (parse "{ 1: 1, 1: 1 }");
    ]
    ast

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "parser test",
        [
          test_case "parse LetStatement" `Slow test_let_statements;
          test_case "parse ReturnStatement" `Slow test_return_statements;
          test_case "parse IdentifireExpression" `Slow
            test_identifier_expression;
          test_case "parse IntegerLiteralExpression" `Slow
            test_integer_literal_expression;
          test_case "parse BooleanLiteralExpression" `Slow
            test_boolean_literal_expression;
          test_case "parse StringLiteralExpression" `Slow
            test_string_literal_expression;
          test_case "parse ArrayLiteralExpression" `Slow
            test_array_literal_expression;
          test_case "parse HashLiteralExpression" `Slow
            test_hash_literal_expression;
          test_case "parse ArrayIndexExpression" `Slow
            test_array_index_expression;
          test_case "parse PrefixExpression" `Slow test_prefix_expression;
          test_case "parse InfixExpression" `Slow test_infix_expression;
          test_case "parse grouped Expression" `Slow test_grouped_expression;
          test_case "parse if Expression" `Slow test_if_expression;
          test_case "parse if-else Expression" `Slow test_if_else_expression;
          test_case "parse function literal Expression" `Slow
            test_function_literal_expression;
          test_case "parse call Expression" `Slow test_call_expression;
          test_case "modify Expression" `Slow test_modify;
        ] );
    ]
