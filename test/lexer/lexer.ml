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
  let code =
    "let five = 5;\n\
     let ten = 10;\n\n\
     let add = fn(x, y) {\n\
    \  x + y;\n\
     };\n\n\
     let result = add(five, ten);\n\
     !-/*5;\n\
     5 < 10 > 5;\n\n\
     if (5 < 10) {\n\
    \  return true;\n\
     } else {\n\
    \  return false;\n\
     }\n\n\
     10 == 10;\n\
     10 != 9;\n\
     \"foobar\"\n\
     \"foo bar\"\n" in
  let check = Alcotest.(check (list token_testable)) in
  let open Token in
  check "same token"
    [
      Let;
      Ident "five";
      Assign;
      Int 5;
      Semicolon;
      Let;
      Ident "ten";
      Assign;
      Int 10;
      Semicolon;
      Let;
      Ident "add";
      Assign;
      Function;
      LParen;
      Ident "x";
      Comma;
      Ident "y";
      RParen;
      LBrace;
      Ident "x";
      Plus;
      Ident "y";
      Semicolon;
      RBrace;
      Semicolon;
      Let;
      Ident "result";
      Assign;
      Ident "add";
      LParen;
      Ident "five";
      Comma;
      Ident "ten";
      RParen;
      Semicolon;
      Bang;
      Minus;
      Slash;
      Asterisk;
      Int 5;
      Semicolon;
      Int 5;
      LT;
      Int 10;
      GT;
      Int 5;
      Semicolon;
      If;
      LParen;
      Int 5;
      LT;
      Int 10;
      RParen;
      LBrace;
      Return;
      True;
      Semicolon;
      RBrace;
      Else;
      LBrace;
      Return;
      False;
      Semicolon;
      RBrace;
      Int 10;
      Eq;
      Int 10;
      Semicolon;
      Int 10;
      Not_Eq;
      Int 9;
      Semicolon;
      String "foobar";
      String "foo bar";
      EOF;
    ]
    (code_to_tokens code)

let () =
  let open Alcotest in
  run "Lexer" [("nextToken", [test_case "next token" `Slow test_next_token])]
