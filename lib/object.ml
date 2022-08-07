module Hash = Map.Make (String)

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
  | Builtin  of { fn : t list -> t [@equal fun _ _ -> false] }
  | Array    of t list
  | Hash     of (hash_pair Hash.t[@opaque])
  | Quote    of Ast.expression
  | Macro    of macro
  | Error    of string
[@@deriving show, eq]

and macro = {
  parameters : Ast.identifier list;
  body : Ast.blockStatement;
  env : t Environment.t;
}

and hash_pair = {
  key : t;
  value : t;
}

and option_t = t option [@@deriving show, eq]

let is_hashable = function
  | Integer _
  | Boolean _
  | String _ ->
    true
  | _ -> false

let empty_hash = Hash.empty
let add_hash key value hash = Hash.add (key |> show) { key; value } hash

let decode_tag_of = function
  | Integer _ -> "Integer"
  | Boolean _ -> "Boolean"
  | String _ -> "String"
  | Null -> "Null"
  | Return _ -> "Return"
  | Function _ -> "Function"
  | Builtin _ -> "Builtin"
  | Array _ -> "Array"
  | Hash _ -> "Hash"
  | Quote _ -> "Quote"
  | Macro _ -> "Macro"
  | Error _ -> "Error"

let compare x y = compare (show x) (show y)
