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

let next_token lex =
  let token =
    match lex.ch with
    | '=' -> Token.Assign
    | ';' -> Token.Semicolon
    | '(' -> Token.LParen
    | ')' -> Token.RParen
    | ',' -> Token.Comma
    | '+' -> Token.Plus
    | '{' -> Token.LBrace
    | '}' -> Token.RBrace
    | _ -> Token.EOF in
  (read_char lex, token)
