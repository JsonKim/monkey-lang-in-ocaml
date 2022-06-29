type node =
  | Statement  of statement
  | Expression of expression
[@@deriving show, eq]

and statement =
  | LetStatement        of {
      identifier : expression;
      value : expression;
    }
  | ReturnStatement     of { value : expression }
  | ExpressionStatement of { expression : expression }
[@@deriving show]

and expression =
  | Empty
  | Identifier     of { value : string }
  | IntegerLiteral of { value : int }
  | Boolean        of { value : bool }
  | Prefix         of {
      token : Token.t;
      right : expression;
    }
  | Infix          of {
      token : Token.t;
      left : expression;
      right : expression;
    }
[@@deriving show]

type program = Program of statement list

let show_program p =
  if List.length p > 0 then show_statement (List.hd p) else ""
