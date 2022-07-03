open Ast

let rec eval node =
  match node with
  | ExpressionStatement { expression } -> eval_expression expression
  | _ -> Object.Null

and eval_expression = function
  | Literal (Integer n) -> Object.Integer n
  | _ -> Object.Null

let eval_program program =
  let rec go last_result = function
    | [] -> last_result
    | h :: t -> go (eval h) t in
  go Object.Null program
