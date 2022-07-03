type node =
  | Statement  of statement
  | Expression of expression
[@@deriving show, eq]

and statement =
  | LetStatement        of {
      identifier : identifier;
      value : expression;
    }
  | ReturnStatement     of { value : expression }
  | ExpressionStatement of { expression : expression }
[@@deriving show]

and blockStatement = statement list [@@deriving show]
and identifier = string

and literal =
  | Integer of int
  | Boolean of bool
[@@deriving show]

and expression =
  | Empty
  | Identifier of identifier
  | Literal    of literal
  | Prefix     of {
      token : Token.t;
      right : expression;
    }
  | Infix      of {
      token : Token.t;
      left : expression;
      right : expression;
    }
  | If         of {
      condition : expression;
      consequence : blockStatement;
      alternative : blockStatement option;
    }
  | Function   of {
      parameters : identifier list;
      body : blockStatement;
    }
  | Call       of {
      fn : expression;
      arguments : expression list;
    }
[@@deriving show]

let int_to_literal x = Literal (Integer x)
let bool_to_literal x = Literal (Boolean x)

type program = Program of statement list

let show_program p =
  if List.length p > 0 then show_statement (List.hd p) else ""
