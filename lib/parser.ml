type t = {
  l : Lexer.t;
  cur_token : Token.t;
  peek_token : Token.t;
  errors : string list;
}

let prefixPrecedence = 6
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
  | Token.LParen -> 7
  | Token.LBracket -> 8
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

let parse_identifier p =
  match p.cur_token with
  | Token.Ident value -> (p, Some (Ast.Identifier value))
  | _ ->
    ( parse_error
        ("parse error: not identifier, cur_token: " ^ Token.show p.cur_token)
        p,
      None )

let parse_int value p = (p, Some (value |> Ast.int_to_literal))
let parse_boolean value p = (p, Some (value |> Ast.bool_to_literal))
let parse_string value p = (p, Some (value |> Ast.string_to_literal))

let rec parse_let_statement p =
  let p, is_expected_ident = expect_token (Token.Ident "") p in
  match (is_expected_ident, p.cur_token) with
  | true, Token.Ident identifier ->
    let p, is_expected_assign = expect_token Token.Assign p in
    if is_expected_assign then
      let p = next_token p in
      match parse_expression lowest p with
      | p, Some value ->
        let p = if peek_token_is Token.Semicolon p then p |> next_token else p in
        (p, Some (Ast.LetStatement { identifier; value }))
      | p, None -> (p, None)
    else
      (p, None)
  | _ -> (p, None)

and parse_return_statement p =
  let p = p |> next_token in
  match parse_expression lowest p with
  | p, Some value ->
    let p = if peek_token_is Token.Semicolon p then p |> next_token else p in
    (p, Some (Ast.ReturnStatement { value }))
  | p, None -> (p, None)

and prefix_parse_fns p =
  match p.cur_token with
  | Token.Ident _ -> parse_identifier p
  | Token.Int value -> parse_int value p
  | Token.True -> parse_boolean true p
  | Token.False -> parse_boolean false p
  | Token.String value -> parse_string value p
  | Token.Bang
  | Token.Minus ->
    parse_prefix_expression p
  | Token.LParen -> parse_grouped_expression p
  | Token.If -> parse_if_expression p
  | Token.Function -> parse_function_literal p
  | Token.LBracket -> (
    match parse_expression_list Token.RBracket p with
    | p, Some arr -> (p, Some (arr |> Ast.array_to_literal))
    | p, None -> (p, None))
  | Token.LBrace -> parse_hash p
  | _ ->
    ( parse_error
        ("parse error: prefix not found, cur_token: " ^ Token.show p.cur_token)
        p,
      None )

and parse_grouped_expression p =
  let p = next_token p in
  match parse_expression lowest p with
  | p, None -> (p, None)
  | p, Some exp ->
    let p, is_expected = expect_token Token.RParen p in
    if is_expected then
      (p, Some exp)
    else
      ( parse_error
          ("parse error: right paren not found, cur_token: "
          ^ Token.show p.cur_token)
          p,
        None )

and parse_if_expression p =
  let p, is_expected = expect_token Token.LParen p in
  if is_expected then
    match p |> next_token |> parse_expression lowest with
    | p, Some condition ->
      let p, is_expected = expect_token Token.RParen p in
      if is_expected then
        let p, is_expected = expect_token Token.LBrace p in
        if is_expected then
          match parse_block_statement p with
          | p, None -> (p, None)
          | p, Some consequence ->
            let p, alternative =
              if peek_token_is Token.Else p then
                let p, is_expected =
                  p |> next_token |> expect_token Token.LBrace in
                if is_expected then parse_block_statement p else (p, None)
              else
                (p, None) in
            (p, Some (Ast.If { condition; consequence; alternative }))
        else
          (p, None)
      else
        (p, None)
    | p, None -> (p, None)
  else
    (p, None)

and parse_function_parameters p =
  let rec go parameters p =
    match p.peek_token with
    | Token.Comma -> (
      let p = p |> next_token |> next_token in
      match p.cur_token with
      | Token.Ident value -> go (parameters @ [value]) p
      | _ -> (p, None))
    | Token.RParen -> (p |> next_token, Some parameters)
    | _ -> (p, None) in

  if peek_token_is Token.RParen p then
    (p |> next_token, Some [])
  else
    let p = p |> next_token in
    match p.cur_token with
    | Token.Ident value -> go [value] p
    | _ -> (p, None)

and parse_function_literal p =
  let p, is_expected = expect_token Token.LParen p in
  if is_expected then
    match parse_function_parameters p with
    | p, Some parameters ->
      let p, is_expected = expect_token Token.LBrace p in
      if is_expected then
        match parse_block_statement p with
        | p, Some body -> (p, Some (Ast.Function { parameters; body }))
        | p, None -> (p, None)
      else
        (p, None)
    | p, None -> (p, None)
  else
    (p, None)

and parse_block_statement p =
  let rec go statements p =
    if cur_token_is Token.RBrace p || cur_token_is Token.EOF p then
      (p, statements)
    else
      match parse_statement p with
      | p, None -> go statements (next_token p)
      | p, Some stmt -> go (statements @ [stmt]) (next_token p) in

  let p = next_token p in
  let p, statements = go [] p in
  (p, Some statements)

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
  | Token.LParen -> Some parse_call_expression
  | Token.LBracket -> Some parse_index
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

and parse_call_expression fn p =
  match parse_expression_list Token.RParen p with
  | p, Some arguments -> (p, Some (Ast.Call { fn; arguments }))
  | p, None -> (p, None)

and parse_index left p =
  match parse_expression lowest (p |> next_token) with
  | p, Some index -> (
    match expect_token Token.RBracket p with
    | p, true -> (p, Some (Ast.Index { left; index }))
    | p, false -> (p, None))
  | p, None -> (p, None)

and parse_expression_list end_token p =
  let rec go arr p =
    match p.peek_token with
    | Token.Comma ->
      let p = p |> next_token |> next_token in
      parse_element arr p
    | peek when Token.equal peek end_token -> (p |> next_token, Some arr)
    | _ -> (p, None)
  and parse_element elements p =
    match parse_expression lowest p with
    | p, Some element -> go (elements @ [element]) p
    | p, None -> (p, None) in

  if peek_token_is end_token p then
    (p |> next_token, Some [])
  else
    let p = p |> next_token in
    parse_element [] p

and parse_hash p =
  let rec go acc p =
    match p.peek_token with
    | Token.Comma ->
      let p = p |> next_token |> next_token in
      parse_key_value acc p
    | Token.RBrace -> (p |> next_token, Some (acc |> Ast.hash_to_literal))
    | _ -> (p, None)
  and parse_key_value acc p =
    match parse_expression lowest p with
    | p, Some key -> (
      match expect_token Token.Colon p with
      | p, true -> (
        match parse_expression lowest (p |> next_token) with
        | p, Some value ->
          go (acc |> List.remove_assoc key |> List.cons (key, value)) p
        | p, None -> (p, None))
      | p, false -> (p, None))
    | p, None -> (p, None) in

  match expect_token Token.RBrace p with
  | p, true -> (p, Some ([] |> Ast.hash_to_literal))
  | p, false -> parse_key_value [] (p |> next_token)

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

and parse_expression_statement p =
  match parse_expression lowest p with
  | p, Some expression ->
    let p = if peek_token_is Token.Semicolon p then next_token p else p in
    (p, Some (Ast.ExpressionStatement { expression }))
  | p, None -> (p, None)

and parse_statement p =
  match p.cur_token with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | _ -> parse_expression_statement p

let parse_program p =
  let rec go acc p' =
    match p'.cur_token with
    | Token.EOF -> (p', Ast.Program acc)
    | _ ->
      let next_p, stmt = parse_statement p' in
      let acc' =
        match stmt with
        | Some stmt -> acc @ [stmt]
        | None -> acc in
      go acc' (next_token next_p) in
  go [] p
