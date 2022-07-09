let null_byte = '\x00'

type t = {
  input : string;
  position : int;
  read_position : int;
  ch : char;
}

let peek_char lex =
  if lex.read_position >= String.length lex.input then
    null_byte
  else
    String.get lex.input lex.read_position

let read_char lex =
  let ch = peek_char lex in
  {
    lex with
    position = lex.read_position;
    read_position = lex.read_position + 1;
    ch;
  }

let make input =
  read_char { input; position = 0; read_position = 0; ch = null_byte }

let is_letter ch =
  ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'

let read_identifier lex =
  let rec go acc lex =
    if is_letter lex.ch then
      go (acc ^ String.make 1 lex.ch) (read_char lex)
    else
      (acc, lex) in
  go "" lex

let is_digit ch = '0' <= ch && ch <= '9'

let read_number lex =
  let rec go acc lex =
    if is_digit lex.ch then
      go (acc ^ String.make 1 lex.ch) (read_char lex)
    else
      (int_of_string acc, lex) in
  go "" lex

let read_string lex =
  let rec go lex acc =
    if lex.ch == null_byte || lex.ch == '\n' then
      (lex |> read_char, Token.Illegal "string must end with \". ")
    else if lex.ch == '"' then
      (lex |> read_char, Token.String acc)
    else
      go (read_char lex) (acc ^ String.make 1 lex.ch) in
  if lex.ch == '"' then
    go (lex |> read_char) ""
  else
    ( read_char lex,
      Token.Illegal
        ("string must start with \". current token: " ^ String.make 1 lex.ch) )

let rec skip_whitespace lex =
  match lex.ch with
  | ' '
  | '\t'
  | '\n'
  | '\r' ->
    skip_whitespace (read_char lex)
  | _ -> lex

let next_token l =
  let lex = skip_whitespace l in
  match lex.ch with
  | '=' ->
    if peek_char lex == '=' then
      (read_char (read_char lex), Token.Eq)
    else
      (read_char lex, Token.Assign)
  | '+' -> (read_char lex, Token.Plus)
  | '-' -> (read_char lex, Token.Minus)
  | '!' ->
    if peek_char lex == '=' then
      (read_char (read_char lex), Token.Not_Eq)
    else
      (read_char lex, Token.Bang)
  | '/' -> (read_char lex, Token.Slash)
  | '*' -> (read_char lex, Token.Asterisk)
  | '<' -> (read_char lex, Token.LT)
  | '>' -> (read_char lex, Token.GT)
  | ';' -> (read_char lex, Token.Semicolon)
  | ':' -> (read_char lex, Token.Colon)
  | '(' -> (read_char lex, Token.LParen)
  | ')' -> (read_char lex, Token.RParen)
  | ',' -> (read_char lex, Token.Comma)
  | '{' -> (read_char lex, Token.LBrace)
  | '}' -> (read_char lex, Token.RBrace)
  | '[' -> (read_char lex, Token.LBracket)
  | ']' -> (read_char lex, Token.RBracket)
  | '"' -> read_string lex
  | '\x00' -> (read_char lex, Token.EOF)
  | _ ->
    if is_letter lex.ch then
      let literal, lex = read_identifier lex in
      (lex, Token.keywords literal)
    else if is_digit lex.ch then
      let literal, lex = read_number lex in
      (lex, Token.Int literal)
    else
      (read_char lex, Token.Illegal (String.make 1 lex.ch))
