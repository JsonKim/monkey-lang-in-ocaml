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
  | Infix { token; left; right } ->
    eval_infix token (eval_expression left) (eval_expression right)
  | _ -> Object.Null

and eval_prefix token right =
  match token with
  | Token.Bang -> eval_bang right
  | Token.Minus -> eval_minus right
  | _ -> Object.Null

and eval_infix token left right =
  match (left, right) with
  | Integer l, Integer r -> eval_integer_infix token l r
  | _ -> Object.Null

and eval_integer_infix token left right =
  match token with
  | Token.Plus -> Object.Integer (left + right)
  | Token.Minus -> Object.Integer (left - right)
  | Token.Asterisk -> Object.Integer (left * right)
  | Token.Slash -> Object.Integer (left / right)
  | Token.LT -> if left < right then trueObject else falseObject
  | Token.GT -> if left > right then trueObject else falseObject
  | Token.Eq -> if left == right then trueObject else falseObject
  | Token.Not_Eq -> if left != right then trueObject else falseObject
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