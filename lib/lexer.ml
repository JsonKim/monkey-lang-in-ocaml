let null_byte = '\x00'

type t = {
  input : string;
  position : int;
  read_position : int;
  ch : char;
}

let read_char lex =
  let ch =
    if lex.read_position >= String.length lex.input then
      null_byte
    else
      String.get lex.input lex.read_position in
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
  | '=' -> (read_char lex, Token.Assign)
  | '+' -> (read_char lex, Token.Plus)
  | '-' -> (read_char lex, Token.Minus)
  | '!' -> (read_char lex, Token.Bang)
  | '/' -> (read_char lex, Token.Slash)
  | '*' -> (read_char lex, Token.Asterisk)
  | '<' -> (read_char lex, Token.LT)
  | '>' -> (read_char lex, Token.GT)
  | ';' -> (read_char lex, Token.Semicolon)
  | '(' -> (read_char lex, Token.LParen)
  | ')' -> (read_char lex, Token.RParen)
  | ',' -> (read_char lex, Token.Comma)
  | '{' -> (read_char lex, Token.LBrace)
  | '}' -> (read_char lex, Token.RBrace)
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
