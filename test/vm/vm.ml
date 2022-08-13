open Monkey

exception Compile_Failed

let object_testable = Alcotest.testable Object.pp Object.equal

let parse input =
  let compiler = Compiler.Compiler.empty in
  let ast = input |> Lexer.make |> Parser.make |> Parser.parse_program |> snd in
  match Compiler.Compiler.compile compiler ast with
  | Ok (c, _) -> (
    let bytecode = Compiler.Compiler.to_bytecode c in
    let obj = bytecode |> Vm.make |> Vm.run |> Vm.last_popped_stack_elem in
    match obj with
    | Some obj -> obj
    | None -> raise Compile_Failed)
  | Error _ -> raise Compile_Failed

let object_to_integer obj =
  match obj with
  | Object.Integer n -> n
  | _ -> raise Not_found

let object_to_boolean obj =
  match obj with
  | Object.Boolean b -> b
  | _ -> raise Not_found

let test_integers () =
  let open Alcotest in
  check (list int) "same object"
    ([
       "1";
       "2";
       "1 + 2";
       "1 - 2";
       "1 * 2";
       "4 / 2";
       "50 / 2 * 2 + 10 - 5";
       "5 + 5 + 5 + 5 - 10";
       "2 * 2 * 2 * 2 * 2";
       "5 * 2 + 10";
       "5 + 2 * 10";
       "5 * (2 + 10)";
       "-5";
       "-10";
       "-50 + 100 + -50";
       "(5 + 10 * 2 + 15 / 3) * 2 + -10";
     ]
    |> List.map parse
    |> List.map object_to_integer)
    [1; 2; 3; -1; 2; 2; 55; 10; 32; 20; 25; 60; -5; -10; 0; 50]

let test_boolean_expressions () =
  let open Alcotest in
  check (list bool) "same object"
    ([
       "true";
       "false";
       "1 < 2";
       "1 > 2";
       "1 < 1";
       "1 > 1";
       "1 == 1";
       "1 != 1";
       "1 == 2";
       "1 != 2";
       "true == true";
       "false == false";
       "true == false";
       "true != false";
       "false != true";
       "(1 < 2) == true";
       "(1 < 2) == false";
       "(1 > 2) == true";
       "(1 > 2) == false";
       "!true";
       "!false";
       "!5";
       "!!true";
       "!!false";
       "!!5";
       "!(if (false) { 5; })";
     ]
    |> List.map parse
    |> List.map object_to_boolean)
    [
      true;
      false;
      true;
      false;
      false;
      false;
      true;
      false;
      false;
      true;
      true;
      true;
      false;
      true;
      true;
      true;
      false;
      false;
      true;
      false;
      true;
      false;
      true;
      false;
      true;
      true;
    ]

let test_conditionals () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "if (true) { 10 }";
       "if (true) { 10 } else { 20 }";
       "if (false) { 10 } else { 20 }";
       "if (1) { 10 }";
       "if (1 < 2) { 10 }";
       "if (1 < 2) { 10 } else { 20 }";
       "if (1 > 2) { 10 } else { 20 }";
       "if (1 > 2) { 10 }";
       "if (false) { 10 }";
       "if ((if (false) { 10 })) { 10 } else { 20 }";
     ]
    |> List.map parse)
    [
      Object.Integer 10;
      Object.Integer 10;
      Object.Integer 20;
      Object.Integer 10;
      Object.Integer 10;
      Object.Integer 10;
      Object.Integer 20;
      Object.Null;
      Object.Null;
      Object.Integer 20;
    ]

let test_global_let_statements () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "let one = 1; one";
       "let one = 1; let two = 2; one + two";
       "let one = 1; let two = one + one; one + two";
     ]
    |> List.map parse)
    [Object.Integer 1; Object.Integer 3; Object.Integer 3]

let test_string_expressions () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([{|"monkey"|}; {|"mon" + "key"|}; {|"mon" + "key" + "banana"|}]
    |> List.map parse)
    [
      Object.String "monkey";
      Object.String "monkey";
      Object.String "monkeybanana";
    ]

let test_array_literals () =
  let open Alcotest in
  check (list object_testable) "same object"
    (["[]"; "[1, 2, 3]"; "[1 + 2, 3 * 4, 5 + 6]"] |> List.map parse)
    [
      Object.Array [];
      Object.Array [Object.Integer 1; Object.Integer 2; Object.Integer 3];
      Object.Array [Object.Integer 3; Object.Integer 12; Object.Integer 11];
    ]

let () =
  let open Alcotest in
  run "Code"
    [
      ("integers test", [test_case "integers test" `Slow test_integers]);
      ( "boolean expressions test",
        [test_case "boolean expressions test" `Slow test_boolean_expressions] );
      ( "conditionals test",
        [test_case "conditionals test" `Slow test_conditionals] );
      ( "global let statements test",
        [
          test_case "global let statements test" `Slow test_global_let_statements;
        ] );
      ( "string expressions test",
        [test_case "string expressions test" `Slow test_string_expressions] );
      ( "array literals test",
        [test_case "array literals test" `Slow test_array_literals] );
    ]
