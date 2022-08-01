open Monkey

exception Compile_Failed

let parse input =
  let compiler = Compiler.Compiler.empty in
  let ast = input |> Lexer.make |> Parser.make |> Parser.parse_program |> snd in
  match Compiler.Compiler.compile compiler ast with
  | Some (c, _) -> (
    let bytecode = Compiler.Compiler.to_bytecode c in
    let obj = bytecode |> Vm.make |> Vm.run |> Vm.stack_top in
    match obj with
    | Some obj -> obj
    | None -> raise Compile_Failed)
  | None -> raise Compile_Failed

let object_to_integer obj =
  match obj with
  | Object.Integer n -> n
  | _ -> raise Not_found

let test_integers () =
  let open Alcotest in
  check (list int) "same object"
    (["1"; "2"; "1 + 2"] |> List.map parse |> List.map object_to_integer)
    [1; 2; 3]

let () =
  let open Alcotest in
  run "Code"
    [("integers test", [test_case "integers test" `Slow test_integers])]
