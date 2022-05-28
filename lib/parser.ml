type t = {
  l : Lexer.t;
  cur_token : Token.t;
  peek_token : Token.t;
  errors : string list;
}

let next_token p =
  let l, peek_token = Lexer.next_token p.l in
  { p with l; cur_token = p.peek_token; peek_token }

(*
두 개가 같음
let make l =
  let next_lex, cur_token = Lexer.next_token l in
  let _, peek_token = Lexer.next_token next_lex in
  { l; cur_token; peek_token }
*)

let make l =
  next_token
    (next_token
       { l; cur_token = Token.EOF; peek_token = Token.EOF; errors = [] })

let cur_token_is t p =
  match (p.cur_token, t) with
  | Token.Ident _, Token.Ident _ -> true
  | _ -> p.cur_token == t

let peek_token_is t p =
  match (p.peek_token, t) with
  | Token.Ident _, Token.Ident _ -> true
  | _ -> p.peek_token == t

let peek_error t p =
  let msg =
    "expected next token to be "
    ^ Token.show t
    ^ ", got "
    ^ Token.show p.peek_token
    ^ " instead" in
  { p with errors = p.errors @ [msg] }

let expect_token t p =
  if peek_token_is t p then (next_token p, true) else (peek_error t p, false)

let parse_let_statement p =
  let p, is_expected_ident = expect_token (Token.Ident "") p in
  match (is_expected_ident, p.cur_token) with
  | true, Token.Ident value ->
    let identifier = Ast.Identifier { value } in
    let p, is_expected_assign = expect_token Token.Assign p in
    if is_expected_assign then
      (p, Some (Ast.LetStatement { identifier; value = Ast.Empty }))
    else
      (p, None)
  | _ -> (p, None)

let parse_return_statement p =
  let rec go p' =
    if cur_token_is Token.Semicolon p' then
      p'
    else
      go (next_token p') in
  (go p, Some (Ast.ReturnStatement { value = Ast.Empty }))

let parse_statement p =
  match p.cur_token with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | _ -> (p, None)

let parse_program p =
  let rec go acc p' =
    let next_p, stmt = parse_statement p' in
    let acc' =
      match stmt with
      | Some stmt -> acc @ [stmt]
      | None -> acc in
    match next_p.cur_token with
    | Token.EOF -> acc'
    | _ -> go acc' (next_token next_p) in
  go [] p
