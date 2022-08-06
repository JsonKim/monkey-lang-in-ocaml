open Monkey

exception Compile_Failed

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
     ]
    |> List.map parse
    |> List.map object_to_integer)
    [1; 2; 3; -1; 2; 2; 55; 10; 32; 20; 25; 60]

let test_boolean_expressions () =
  let open Alcotest in
  check (list bool) "same object"
    (["true"; "false"] |> List.map parse |> List.map object_to_boolean)
    [true; false]

let () =
  let open Alcotest in
  run "Code"
    [
      ("integers test", [test_case "integers test" `Slow test_integers]);
      ( "boolean expressions test",
        [test_case "boolean expressions test" `Slow test_boolean_expressions] );
    ]
