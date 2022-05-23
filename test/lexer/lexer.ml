open Monkey

let token_testable = Alcotest.testable Token.pp Token.equal

let code_to_tokens code =
  let rec go lex acc =
    let next_lex, token = Lexer.next_token lex in
    match token with
    | Token.EOF -> acc @ [token]
    | _ -> go next_lex (acc @ [token]) in
  go (Lexer.make code) []

let test_next_token () =
  let code = "=+(){},;" in
  let check = Alcotest.(check (list token_testable)) in
  let open Token in
  check "same token"
    [Assign; Plus; LParen; RParen; LBrace; RBrace; Comma; Semicolon; EOF]
    (code_to_tokens code)

let () =
  let open Alcotest in
  run "Lexer" [("nextToken", [test_case "next token" `Slow test_next_token])]
