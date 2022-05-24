type t =
  | Illegal   of string
  | EOF
  (* 식별자 + 리터럴*)
  | Ident     of string
  | Int       of int
  (* 연산자 *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  (* 구분자 *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* 예약어 *)
  | Function
  | Let
[@@deriving show, eq]

let pp ppf tk = Fmt.pf ppf "Token =%s" (show tk)
