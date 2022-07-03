open Monkey

module To_test = struct
  let eval lex =
    lex |> Parser.make |> Parser.parse_program |> snd |> Evaluator.eval_program
end

let evaluator_testable = Alcotest.testable Object.pp Object.equal

let test_eval () =
  let code = ["10"; "-10"; "!true"; "!!false"; "!5"; "-5 + 5"; "2 * (1 + 2)"] in
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
      ]
    (code |> List.map (fun code -> code |> Lexer.make |> To_test.eval))

let () =
  let open Alcotest in
  run "Parser" [("evaluator test", [test_case "eval" `Slow test_eval])]
