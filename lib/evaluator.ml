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
  | _ -> Object.Null

let eval_program program =
  let rec go last_result = function
    | [] -> last_result
    | h :: t -> go (eval h) t in
  go Object.Null program
