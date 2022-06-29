type t = {
  l : Lexer.t;
  cur_token : Token.t;
  peek_token : Token.t;
  errors : string list;
}

let prefixPrecedence = 7
let lowest = 1

let precedence tok =
  match tok with
  | Token.Eq
  | Token.Not_Eq ->
    2
  | Token.LT
  | Token.GT ->
    3
  | Token.Plus
  | Token.Minus ->
    4
  | Token.Slash
  | Token.Asterisk ->
    5
  | Token.LParen -> 6
  | Token.LBrace -> 8
  | _ -> lowest

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

let peek_precedence p = precedence p.peek_token
let cur_precedence p = precedence p.cur_token

let peek_error t p =
  let msg =
    "expected next token to be "
    ^ Token.show t
    ^ ", got "
    ^ Token.show p.peek_token
    ^ " instead" in
  { p with errors = p.errors @ [msg] }

let parse_error msg p = { p with errors = p.errors @ [msg] }

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
    if p' |> cur_token_is Token.Semicolon then
      p'
    else
      p' |> next_token |> go in
  (p |> next_token |> go, Some (Ast.ReturnStatement { value = Ast.Empty }))

let parse_identifier p =
  match p.cur_token with
  | Token.Ident value -> (p, Some (Ast.Identifier { value }))
  | _ ->
    ( parse_error
        ("parse error: not identifier, cur_token: " ^ Token.show p.cur_token)
        p,
      None )

let parse_int p =
  match p.cur_token with
  | Token.Int value -> (p, Some (Ast.IntegerLiteral { value }))
  | _ ->
    ( parse_error
        ("parse error: not int, cur_token: " ^ Token.show p.cur_token)
        p,
      None )

let parse_boolean p =
  match p.cur_token with
  | Token.True -> (p, Some (Ast.Boolean { value = true }))
  | Token.False -> (p, Some (Ast.Boolean { value = false }))
  | _ ->
    ( parse_error
        ("parse error: not boolean, cur_token: " ^ Token.show p.cur_token)
        p,
      None )

let rec prefix_parse_fns p =
  match p.cur_token with
  | Token.Ident _ -> parse_identifier p
  | Token.Int _ -> parse_int p
  | Token.True
  | Token.False ->
    parse_boolean p
  | Token.Bang
  | Token.Minus ->
    parse_prefix_expression p
  | _ ->
    ( parse_error
        ("parse error: prefix not found, cur_token: " ^ Token.show p.cur_token)
        p,
      None )

and infix_parse_fns p =
  match p.peek_token with
  | Token.Plus
  | Token.Minus
  | Token.Slash
  | Token.Asterisk
  | Token.Eq
  | Token.Not_Eq
  | Token.LT
  | Token.GT ->
    Some parse_infix_expression
  | _ -> None

and parse_prefix_expression p =
  let token = p.cur_token in
  let p = next_token p in
  match parse_expression prefixPrecedence p with
  | p, Some right -> (p, Some (Ast.Prefix { token; right }))
  | p, None -> (p, None)

and parse_infix_expression left p =
  let precedence = cur_precedence p in
  let token = p.cur_token in
  let p = next_token p in
  match parse_expression precedence p with
  | p, Some right -> (p, Some (Ast.Infix { token; left; right }))
  | p, None -> (p, None)

and parse_expression precedence p =
  let rec go p left =
    if
      peek_token_is Token.Semicolon p == false && precedence < peek_precedence p
    then
      match infix_parse_fns p with
      | Some infix -> (
        let p = next_token p in
        let p, left = infix left p in
        match left with
        | Some left -> go p left
        | None -> (p, None))
      | None -> (p, Some left)
    else
      (p, Some left) in
  match prefix_parse_fns p with
  | p, None -> (p, None)
  | p, Some left -> go p left

let parse_expression_statement p =
  match parse_expression lowest p with
  | p, Some expression ->
    let p = if peek_token_is Token.Semicolon p then next_token p else p in
    (p, Some (Ast.ExpressionStatement { expression }))
  | p, None -> (p, None)

let parse_statement p =
  match p.cur_token with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | _ -> parse_expression_statement p

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
