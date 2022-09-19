open Monkey

exception Compile_Failed

let object_testable = Alcotest.testable Object.pp Object.equal

let hash_pair_testable =
  Alcotest.testable Object.pp_hash_pair Object.equal_hash_pair

let parse input =
  let compiler = Compiler.Compiler.make () in
  let ast = input |> Lexer.make |> Parser.make |> Parser.parse_program |> snd in
  match Compiler.Compiler.compile compiler ast with
  | Ok (c, _) -> (
    let bytecode = Compiler.Compiler.to_bytecode c in
    let obj = bytecode |> Vm.make |> Vm.run |> Vm.last_popped_stack_elem in
    match obj with
    | Some obj -> obj
    | None -> raise Compile_Failed)
  | Error _ -> raise Compile_Failed

let parse_with_vm_error input =
  try parse input with
  | Vm.VM_Error message -> Object.Error message

let hash_to_list obj =
  match obj with
  | Object.Hash hash -> hash |> Object.Hash.to_seq |> List.of_seq
  | _ -> raise Not_found

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

let test_hash_literals () =
  let open Alcotest in
  check
    (list (list (pair string hash_pair_testable)))
    "same object"
    (["{}"; "{1: 2, 2: 3}"; "{1 + 1: 2 * 2, 3 + 3: 4 * 4}"]
    |> List.map parse
    |> List.map hash_to_list)
    ([
       Object.empty_hash;
       Object.empty_hash
       |> Object.add_hash (Object.Integer 1) (Object.Integer 2)
       |> Object.add_hash (Object.Integer 2) (Object.Integer 3);
       Object.empty_hash
       |> Object.add_hash (Object.Integer 2) (Object.Integer 4)
       |> Object.add_hash (Object.Integer 6) (Object.Integer 16);
     ]
    |> List.map Object.Hash.to_seq
    |> List.map List.of_seq)

let test_index_expressions () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "[1, 2, 3][1]";
       "[1, 2, 3][0 + 2]";
       "[[1, 1, 1]][0][0]";
       "[][0]";
       "[1, 2, 3][99]";
       "[1][-1]";
       "{1: 1, 2: 2}[1]";
       "{1: 1, 2: 2}[2]";
       "{1: 1}[0]";
       "{}[0]";
     ]
    |> List.map parse)
    [
      Object.Integer 2;
      Object.Integer 3;
      Object.Integer 1;
      Object.Null;
      Object.Null;
      Object.Null;
      Object.Integer 1;
      Object.Integer 2;
      Object.Null;
      Object.Null;
    ]

let test_calling_function_without_arguments () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "let fivePlusTen = fn() { 5 + 10; };\nfivePlusTen();";
       "let one = fn() { 1; };\nlet two = fn() { 2; };\none() + two()";
       "let a = fn() { 1; };\n\
        let b = fn() { a() + 1; };\n\
        let c = fn() { b() + 1; };\n\
        c();";
     ]
    |> List.map parse)
    [Object.Integer 15; Object.Integer 3; Object.Integer 3]

let test_functions_with_return_statements () =
  let open Alcotest in
  check (list object_testable) "same object"
    (["let earlyExit = fn() { return 99; 100; };\nearlyExit();"]
    |> List.map parse)
    [Object.Integer 99]

let test_functions_without_return_value () =
  let open Alcotest in
  check (list object_testable) "same object"
    (["let noReturn = fn() { };\nnoReturn();"] |> List.map parse)
    [Object.Null]

let test_first_class_functions () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "let returnsOne = fn() { 1; };\n\
        let returnsOneReturner = fn() { returnsOne };\n\
        returnsOneReturner()()";
       "let returnsOneReturner = fn() {\n\
       \  let returnsOne = fn() { 1; };\n\
       \  returnsOne;\n\
        }\n\
        returnsOneReturner()()";
     ]
    |> List.map parse)
    [Object.Integer 1; Object.Integer 1]

let test_calling_functions_with_bindings () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "let identity = fn(a) { a };\nidentity(4);";
       "let sum = fn(a, b) { a + b; };\nsum(1, 2);";
       "let sum = fn(a, b) {\n  let c = a + b;\n  c;\n};\nsum(1, 2);";
       "let sum = fn(a, b) {\n\
       \  let c = a + b;\n\
       \  c;\n\
        };\n\
        sum(1, 2) + sum(3, 4);";
       "let sum = fn(a, b) {\n\
       \  let c = a + b;\n\
       \  c;\n\
        };\n\
        let outer = fn() {\n\
       \  sum(1, 2) + sum(3, 4);\n\
        };\n\
        outer();";
       "let globalNum = 10\n\n\
        let sum = fn(a, b) {\n\
       \  let c = a + b;\n\
       \  c + globalNum;\n\
        };\n\n\
        let outer = fn() {\n\
       \  sum(1, 2) + sum(3, 4) + globalNum;\n\
        };\n\n\
        outer() + globalNum;";
     ]
    |> List.map parse)
    [
      Object.Integer 4;
      Object.Integer 3;
      Object.Integer 3;
      Object.Integer 10;
      Object.Integer 10;
      Object.Integer 50;
    ]

let test_calling_functions_with_arguments_and_bindings () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "let one = fn() { let one = 1; one };\none();";
       "let oneAndTwo = fn() { let one = 1; let two = 2; one + two };\n\
        oneAndTwo();";
       "let oneAndTwo = fn() { let one = 1; let two = 2; one + two };\n\
        oneAndTwo();\n\
        let threeAndFour = fn() { let three = 3; let four = 4; three + four };\n\
        oneAndTwo() + threeAndFour();";
       "let firstFoobar = fn() { let foobar = 50; foobar; };\n\
        let secondFoobar = fn() { let foobar = 100; foobar; };\n\
        firstFoobar() + secondFoobar();";
       "let globalSeed = 50;\n\
        let minusOne = fn() { let num = 1; globalSeed - num };\n\
        let minusTwo = fn() { let num = 2; globalSeed - num };\n\
        minusOne() + minusTwo();";
     ]
    |> List.map parse)
    [
      Object.Integer 1;
      Object.Integer 3;
      Object.Integer 10;
      Object.Integer 150;
      Object.Integer 97;
    ]

let test_calling_functions_with_wrong_arguments () =
  let open Alcotest in
  check (list object_testable) "same object"
    (["fn() { 1; }(1);"; "fn(a) { a; }();"; "fn(a, b) { a + b; }(1);"]
    |> List.map parse_with_vm_error)
    [
      Object.Error "wrong number of argements: want=1, got=0";
      Object.Error "wrong number of argements: want=0, got=1";
      Object.Error "wrong number of argements: want=1, got=2";
    ]

let test_builtin_functions () =
  let open Alcotest in
  check (list object_testable) "same object"
    ([
       "len(\"\")";
       "len(\"four\")";
       "len(\"hello world\")";
       "len(1)";
       "len(\"one\", \"two\")";
       "len([4, 5, 6])";
       "len([])";
       "puts(\"hello\", \"world!\")";
       "first([1, 2, 3])";
       "first([])";
       "first(1)";
       "first([1, 2, 3])";
       "last([1, 2, 3])";
       "last([])";
       "last(1)";
       "rest([1, 2, 3])";
       "rest([])";
       "push([], 1)";
       "push(1, 1)";
     ]
    |> List.map parse_with_vm_error)
    [
      Object.Integer 0;
      Object.Integer 4;
      Object.Integer 11;
      Object.Error "argument to len not supported, got Integer";
      Object.Error "wrong number of arguments. got=2, want=1";
      Object.Integer 3;
      Object.Integer 0;
      Object.Null;
      Object.Integer 1;
      Object.Null;
      Object.Error "argument to first must be Array, got Integer";
      Object.Integer 1;
      Object.Integer 3;
      Object.Null;
      Object.Error "argument to last must be Array, got Integer";
      Object.Array [Object.Integer 2; Object.Integer 3];
      Object.Null;
      Object.Array [Object.Integer 1];
      Object.Error "argument to rest must be Array, got Integer";
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
      ( "hash literals test",
        [test_case "hash literals test" `Slow test_hash_literals] );
      ( "index expressions test",
        [test_case "index expressions test" `Slow test_index_expressions] );
      ( "calling function without arguments test",
        [
          test_case "calling function without arguments test" `Slow
            test_calling_function_without_arguments;
        ] );
      ( "functions with return statements",
        [
          test_case "functions with return statements" `Slow
            test_functions_with_return_statements;
        ] );
      ( "functions without return value",
        [
          test_case "functions without return value" `Slow
            test_functions_without_return_value;
        ] );
      ( "functions first class functions",
        [
          test_case "functions first class functions" `Slow
            test_first_class_functions;
        ] );
      ( "calling functions with bindings test",
        [
          test_case "calling functions with bindings test" `Slow
            test_calling_functions_with_bindings;
        ] );
      ( "calling functions with arguments and bindings test",
        [
          test_case "calling functions with arguments and bindings test" `Slow
            test_calling_functions_with_arguments_and_bindings;
        ] );
      ( "calling functions with wrong arguments",
        [
          test_case "calling functions with wrong arguments" `Slow
            test_calling_functions_with_wrong_arguments;
        ] );
      ( "builtin functions test",
        [test_case "builtin functions test" `Slow test_builtin_functions] );
    ]
