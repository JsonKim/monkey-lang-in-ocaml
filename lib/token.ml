type t =
  | Illegal   of string
  | EOF
  (* 식별자 + 리터럴*)
  | Ident     of string
  | Int       of int
  | String    of string
  (* 연산자 *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | Eq
  | Not_Eq
  (* 구분자 *)
  | Comma
  | Semicolon
  | Colon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  (* 예약어 *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show, eq]

let keywords literal =
  match literal with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Ident literal
