open Monkey

let compile_testable =
  Alcotest.testable Compiler.Compiler.pp Compiler.Compiler.equal

let concat_bytes l =
  List.fold_right (fun b acc -> Bytes.cat b acc) l Bytes.empty

let parser code =
  code |> Lexer.make |> Parser.make |> Parser.parse_program |> snd

let test_integer_arithmetic () =
  let open Alcotest in
  let open Code.OpCode in
  let open Compiler.Compiler in
  check (option compile_testable) "same object"
    (Some
       {
         Compiler.Compiler.instructions =
           concat_bytes [Code.make OpConstant [0]; Code.make OpConstant [1]];
         constants = [|Object.Integer 1; Object.Integer 2|];
       })
    ("1 + 2" |> parser |> compile empty |> Option.map fst)

let () =
  let open Alcotest in
  run "Parser"
    [("make test", [test_case "make test" `Slow test_integer_arithmetic])]
