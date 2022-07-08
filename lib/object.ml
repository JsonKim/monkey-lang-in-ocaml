type t =
  | Integer  of int
  | Boolean  of bool
  | String   of string
  | Null
  | Return   of t
  | Function of {
      parameters : Ast.identifier list;
      body : Ast.blockStatement;
      env : t Environment.t;
    }
  | Error    of string
[@@deriving show, eq]

let decode_tag_of = function
  | Integer _ -> "Integer"
  | Boolean _ -> "Boolean"
  | String _ -> "String"
  | Null -> "Null"
  | Return _ -> "Return"
  | Function _ -> "Function"
  | Error _ -> "Error"

let compare x y = compare (show x) (show y)
