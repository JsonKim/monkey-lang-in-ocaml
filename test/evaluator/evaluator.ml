open Monkey

exception Not_Program

module To_test = struct
  let eval lex =
    lex
    |> Parser.make
    |> Parser.parse_program
    |> snd
    |> Evaluator.eval (Environment.make ())
    |> fst

  let eval_for_error lex =
    lex
    |> Parser.make
    |> Parser.parse_program
    |> snd
    |> (function
         | Ast.Program p -> p
         | _ -> raise Not_Program)
    |> Evaluator.eval_block_statement (Environment.make ())
    |> fst
end

let evaluator_testable = Alcotest.testable Object.pp Object.equal

let test_eval () =
  let code =
    [
      "10";
      "-10";
      "!true";
      "!!false";
      "!5";
      "-5 + 5";
      "2 * (1 + 2)";
      "1 < 2";
      "2 < 1";
      "1 == 1";
      "1 != 1";
      "(1 + 1) == 2";
      "(2 < 1) == false";
      "(1 + 1) != 3";
      "if (true) { 10 }";
      "if (false) { 10 }";
      "if (1 == 2) { 10 } else { 20 }";
      "true; return 2 * 3; 10 + 20";
      {|
      if (10 > 1) {
        if (10 > 1) {
          return 10;
        }

        return 1;
      }
      |};
      "let x = 5; x;";
      "let x = 5 * 5; x;";
      "let x = 5; let y = x; y";
      "\"hello world\"";
      "\"hello\" + \" \" + \"world\"";
      "[1, 2, 3]";
      "[1 + 2, 3 * 4, [1,2,3]]";
      "[1,2,3][0]";
      "[1,2,3][1]";
      "[1,2,3][2]";
      "let i = 0; [1][i];";
      "[1,2,3][1 + 1]";
      "let myArray = [1,2,3]; myArray[2];";
      "let myArray = [1,2,3]; myArray[0] + myArray[1] + myArray[2];";
      "let myArray = [1,2,3]; let i = myArray[0]; myArray[i]";
      "[1,2,3][3]";
      "[1,2,3][-1]";
      {|
        let two = "two";
        {
          "one": 10 - 9,
          two: 1 + 1,
          "thr" + "ee": 6 / 2,
          4: 4,
          true: 5,
          false: 6
        }
      |};
      {|{"foo":5}["foo"]|};
      {|{"foo":5}["bar"]|};
      {|let key = "foo"; {"foo":5}[key]|};
      {|{}["foo"]|};
      {|{5: 5}[5]|};
      {|{true: 5}[true]|};
      {|{false: 5}[false]|};
    ] in
  Alcotest.(check (list evaluator_testable))
    "same object"
    Object.
      [
        Integer 10;
        Integer (-10);
        Boolean false;
        Boolean false;
        Boolean false;
        Integer 0;
        Integer 6;
        Boolean true;
        Boolean false;
        Boolean true;
        Boolean false;
        Boolean true;
        Boolean true;
        Boolean true;
        Integer 10;
        Null;
        Integer 20;
        Integer 6;
        Integer 10;
        Integer 5;
        Integer 25;
        Integer 5;
        String "hello world";
        String "hello world";
        Array [Integer 1; Integer 2; Integer 3];
        Array [Integer 3; Integer 12; Array [Integer 1; Integer 2; Integer 3]];
        Integer 1;
        Integer 2;
        Integer 3;
        Integer 1;
        Integer 3;
        Integer 3;
        Integer 6;
        Integer 2;
        Null;
        Null;
        Hash
          (empty_hash
          |> add_hash (String "one") (Integer 1)
          |> add_hash (String "two") (Integer 2)
          |> add_hash (String "three") (Integer 3)
          |> add_hash (Integer 4) (Integer 4)
          |> add_hash (Boolean true) (Integer 5)
          |> add_hash (Boolean false) (Integer 6));
        Integer 5;
        Null;
        Integer 5;
        Null;
        Integer 5;
        Integer 5;
        Integer 5;
      ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval))

let test_error () =
  let code =
    [
      "5 + true";
      "5 + true; 5";
      "-true";
      "true + false";
      "5; true + false; 5";
      "if (10 > 1) { true + false; }";
      "(-true) + 3";
      "3 - (-false)";
      "-(-false)";
      "if (10 > true) { 1 + 2; }";
      "return -(-true)";
      "\"hello\" - \"world\"";
    ] in
  Alcotest.(check (list evaluator_testable))
    "same object"
    Object.
      [
        Error "type mismatch: Integer Token.Plus Boolean";
        Error "type mismatch: Integer Token.Plus Boolean";
        Error "unknown operator: -(Object.Boolean true)";
        Error
          "unknown operator: (Object.Boolean true) Token.Plus (Object.Boolean \
           false)";
        Error
          "unknown operator: (Object.Boolean true) Token.Plus (Object.Boolean \
           false)";
        Error
          "unknown operator: (Object.Boolean true) Token.Plus (Object.Boolean \
           false)";
        Error "unknown operator: -(Object.Boolean true)";
        Error "unknown operator: -(Object.Boolean false)";
        Error "unknown operator: -(Object.Boolean false)";
        Error "type mismatch: Integer Token.GT Boolean";
        Error "unknown operator: -(Object.Boolean true)";
        Error "unknown operator: hello Token.Minus world";
      ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let test_function_object () =
  let code = ["fn(x) { x + 2; }"] in
  Alcotest.(check (list evaluator_testable))
    "same object"
    [
      Object.Function
        {
          parameters = ["x"];
          body =
            [
              Ast.ExpressionStatement
                {
                  expression =
                    Ast.Infix
                      {
                        token = Token.Plus;
                        left = Ast.Identifier "x";
                        right = Ast.Literal (Ast.Integer 2);
                      };
                };
            ];
          env = { Environment.env = []; outer = None };
          name = "";
        };
    ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let test_function_application () =
  let code =
    [
      "let identity = fn(x) { x; }; identity(5);";
      "let identity = fn(x) { return x; }; identity(5);";
      "let double = fn(x) { x * 2 }; double(5)";
      "let add = fn(x, y) { x + y; }; add(3, 7)";
      "let add = fn(x, y) { x + y; }; add(add(2, 3), add(4,5));";
      "len(\"test\");";
      "len([1,2,3]);";
      "first([42, 7]);";
      "first([]);";
      "last([42, 7]);";
      "rest([1,2,3]);";
      "push([1,2,3], 4);";
    ] in
  Alcotest.(check (list evaluator_testable))
    "same object"
    [
      Object.Integer 5;
      Object.Integer 5;
      Object.Integer 10;
      Object.Integer 10;
      Object.Integer 14;
      Object.Integer 4;
      Object.Integer 3;
      Object.Integer 42;
      Object.Null;
      Object.Integer 7;
      Object.Array [Object.Integer 2; Object.Integer 3];
      Object.Array
        [Object.Integer 1; Object.Integer 2; Object.Integer 3; Object.Integer 4];
    ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let test_closures () =
  let code =
    [
      "let newAdder = fn(x) { fn(y) { x + y; } }; let addTwo = newAdder(2); \
       addTwo(3);";
    ] in
  Alcotest.(check (list evaluator_testable))
    "same object" [Object.Integer 5]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let test_quote () =
  let code = ["quote(5 + 8)"; "quote(foobar)"; "quote(foobar + barfoo)"] in
  Alcotest.(check (list evaluator_testable))
    "same object"
    [
      Object.Quote
        (Ast.Infix
           {
             token = Token.Plus;
             left = Ast.Literal (Ast.Integer 5);
             right = Ast.Literal (Ast.Integer 8);
           });
      Object.Quote (Ast.Identifier "foobar");
      Object.Quote
        (Ast.Infix
           {
             token = Token.Plus;
             left = Ast.Identifier "foobar";
             right = Ast.Identifier "barfoo";
           });
    ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let test_quote_unqute () =
  let code =
    [
      "quote(unquote(4))";
      "quote(unquote(4 + 4))";
      "quote(8 + unquote(4 + 4))";
      "quote(unquote(4 + 4) + 8)";
      "let foobar = 8; quote(foobar)";
      "let foobar = 8; quote(unquote(foobar))";
      "quote(unquote(true))";
      "quote(unquote(true == false))";
      "quote(unquote(quote(4 + 4)))";
      "let quotedInfixExpression = quote(4 + 4);\n\
       quote(unquote(4 + 4) + unquote(quotedInfixExpression))";
    ] in
  Alcotest.(check (list evaluator_testable))
    "same object"
    [
      Object.Quote (Ast.Literal (Ast.Integer 4));
      Object.Quote (Ast.Literal (Ast.Integer 8));
      Object.Quote
        (Ast.Infix
           {
             token = Token.Plus;
             left = Ast.Literal (Ast.Integer 8);
             right = Ast.Literal (Ast.Integer 8);
           });
      Object.Quote
        (Ast.Infix
           {
             token = Token.Plus;
             left = Ast.Literal (Ast.Integer 8);
             right = Ast.Literal (Ast.Integer 8);
           });
      Object.Quote (Ast.Identifier "foobar");
      Object.Quote (Ast.Literal (Ast.Integer 8));
      Object.Quote (Ast.Literal (Ast.Boolean true));
      Object.Quote (Ast.Literal (Ast.Boolean false));
      Object.Quote
        (Ast.Infix
           {
             token = Token.Plus;
             left = Ast.Literal (Ast.Integer 4);
             right = Ast.Literal (Ast.Integer 4);
           });
      Object.Quote
        (Ast.Infix
           {
             token = Token.Plus;
             left = Ast.Literal (Ast.Integer 8);
             right =
               Ast.Infix
                 {
                   token = Token.Plus;
                   left = Ast.Literal (Ast.Integer 4);
                   right = Ast.Literal (Ast.Integer 4);
                 };
           });
    ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let test_recursive_fibonacci () =
  let code =
    [
      "let fibonacci = fn(x) {\n\
      \  if (x == 0) {\n\
      \    return 0;\n\
      \  } else {\n\
      \    if (x == 1) {\n\
      \      return 1;\n\
      \    } else {\n\
      \      fibonacci(x - 1) + fibonacci(x - 2);\n\
      \    }\n\
      \  }\n\
       };\n\
       fibonacci(15);";
    ] in

  Alcotest.(check (list evaluator_testable))
    "same object" [Object.Integer 610]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let () =
  let open Alcotest in
  run "Parser"
    [
      ("evaluator test", [test_case "eval" `Slow test_eval]);
      ( "function object test",
        [test_case "function object" `Slow test_function_object] );
      ( "function application test",
        [test_case "function application" `Slow test_function_application] );
      ("closure test", [test_case "closure" `Slow test_closures]);
      ("error test", [test_case "error" `Slow test_error]);
      ("quote test", [test_case "quote" `Slow test_quote]);
      ("quote-unquote test", [test_case "quote-unquote" `Slow test_quote_unqute]);
      ( "recursive fibonacci test",
        [test_case "recursive fibonacci" `Slow test_recursive_fibonacci] );
    ]
