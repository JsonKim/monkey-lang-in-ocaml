let prompt = ">> "

let print code =
  let rec go lex =
    let next_lex, token = Lexer.next_token lex in
    print_endline (Token.show token);
    match token with
    | Token.EOF -> ()
    | _ -> go next_lex in
  go (Lexer.make code)

let run () =
  print_string prompt;
  let line = read_line () in
  print line
