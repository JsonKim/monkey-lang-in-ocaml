open Monkey

exception Not_Program

module To_test = struct
  let eval lex =
    lex |> Parser.make |> Parser.parse_program |> snd |> Evaluator.eval

  let eval_for_error lex =
    lex
    |> Parser.make
    |> Parser.parse_program
    |> snd
    |> (function
         | Ast.Program p -> p
         | _ -> raise Not_Program)
    |> Evaluator.eval_block_statement
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
      ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval_for_error))

let () =
  let open Alcotest in
  run "Parser"
    [
      ("evaluator test", [test_case "eval" `Slow test_eval]);
      ("error test", [test_case "error" `Slow test_error]);
    ]
