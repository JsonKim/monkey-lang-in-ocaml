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

and macro = {
  parameters : identifier list;
  body : blockStatement;
}

and literal =
  | Integer of int
  | Boolean of bool
  | String  of string
  | Array   of expression list
  | Hash    of (expression * expression) list
  | Macro   of macro
[@@deriving show]

and expression =
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
  | Index      of {
      left : expression;
      index : expression;
    }
[@@deriving show]

let int_to_literal x = Literal (Integer x)
let bool_to_literal x = Literal (Boolean x)
let string_to_literal x = Literal (String x)
let array_to_literal x = Literal (Array x)
let hash_to_literal x = Literal (Hash x)
