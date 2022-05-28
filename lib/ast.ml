type node =
  | Statement  of statement
  | Expression of expression
[@@deriving show, eq]

and statement =
  | LetStatement    of {
      identifier : expression;
      value : expression;
    }
  | ReturnStatement of { value : expression }
[@@deriving show]

and expression =
  | Empty
  | Identifier of { value : string }
[@@deriving show]

type program = Program of statement list

let show_program p =
  if List.length p > 0 then show_statement (List.hd p) else ""
