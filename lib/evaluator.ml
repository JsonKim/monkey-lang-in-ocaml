open Ast

let trueObject = Object.Boolean true
let falseObject = Object.Boolean false

let rec eval node =
  match node with
  | ExpressionStatement { expression } -> eval_expression expression
  | _ -> Object.Null

and eval_expression = function
  | Literal (Integer n) -> Object.Integer n
  | Literal (Boolean true) -> trueObject
  | Literal (Boolean false) -> falseObject
  | Prefix { token; right } -> eval_prefix token (eval_expression right)
  | _ -> Object.Null

and eval_prefix token right =
  match token with
  | Token.Bang -> eval_bang right
  | Token.Minus -> eval_minus right
  | _ -> Object.Null

and eval_bang = function
  | Object.Boolean true -> falseObject
  | Object.Boolean false -> trueObject
  | Object.Null -> trueObject
  | _ -> falseObject

and eval_minus = function
  | Object.Integer n -> Object.Integer (-n)
  | _ -> Object.Null

let eval_program program =
  let rec go last_result = function
    | [] -> last_result
    | h :: t -> go (eval h) t in
  go Object.Null program
