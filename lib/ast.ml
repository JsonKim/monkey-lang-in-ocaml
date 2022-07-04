type node =
  | Statement      of statement
  | Expression     of expression
  | Program        of statement list
  | BlockStatement of blockStatement
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
