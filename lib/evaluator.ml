open Ast

let trueObject = Object.Boolean true
let falseObject = Object.Boolean false

let is_truthy = function
  | Object.Null -> false
  | Object.Boolean b -> b
  | _ -> true

let rec eval_statement node =
  match node with
  | ExpressionStatement { expression } -> eval_expression expression
  | ReturnStatement { value } -> (
    match eval_expression value with
    | Object.Error message -> Object.Error message
    | value -> Object.Return value)
  | _ -> Object.Null

and eval_expression = function
  | Literal (Integer n) -> Object.Integer n
  | Literal (Boolean true) -> trueObject
  | Literal (Boolean false) -> falseObject
  | Prefix { token; right } -> (
    match eval_expression right with
    | Object.Error message -> Object.Error message
    | right -> eval_prefix token right)
  | Infix { token; left; right } -> (
    match eval_expression left with
    | Object.Error message -> Object.Error message
    | left ->
    match eval_expression right with
    | Object.Error message -> Object.Error message
    | right -> eval_infix token left right)
  | If { condition; consequence; alternative } -> (
    match condition |> eval_expression with
    | Object.Error message -> Object.Error message
    | condition -> (
      if condition |> is_truthy then
        eval_block_statement consequence
      else
        match alternative with
        | Some alternative -> eval_block_statement alternative
        | None -> Object.Null))
  | _ -> Object.Null

and eval_prefix token right =
  match token with
  | Token.Bang -> eval_bang right
  | Token.Minus -> eval_minus right
  | _ ->
    Object.Error ("unknown operator: " ^ Token.show token ^ Object.show right)

and eval_infix token left right =
  match (token, left, right) with
  | _, Integer l, Integer r -> eval_integer_infix token l r
  | Token.Eq, _, _ ->
    if Object.equal left right then trueObject else falseObject
  | Token.Not_Eq, _, _ ->
    if Object.equal left right == false then trueObject else falseObject
  | _, left, right when Object.decode_tag_of left != Object.decode_tag_of right
    ->
    Object.Error
      ("type mismatch: "
      ^ Object.decode_tag_of left
      ^ " "
      ^ Token.show token
      ^ " "
      ^ Object.decode_tag_of right)
  | _ ->
    Object.Error
      ("unknown operator: "
      ^ Object.show left
      ^ " "
      ^ Token.show token
      ^ " "
      ^ Object.show right)

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
  | _ ->
    Object.Error
      ("unknown operator: "
      ^ string_of_int left
      ^ " "
      ^ Token.show token
      ^ " "
      ^ string_of_int right)

and eval_bang = function
  | Object.Boolean true -> falseObject
  | Object.Boolean false -> trueObject
  | Object.Null -> trueObject
  | _ -> falseObject

and eval_minus right =
  match right with
  | Object.Integer n -> Object.Integer (-n)
  | _ -> Object.Error ("unknown operator: -" ^ Object.show right)

and eval node =
  match node with
  | Ast.Program program -> eval_program program
  | Ast.BlockStatement block_statement -> eval_block_statement block_statement
  | Ast.Statement statement -> eval_statement statement
  | Ast.Expression expression -> eval_expression expression

and eval_program program =
  let rec go last_result = function
    | [] -> last_result
    | h :: t ->
    match eval_statement h with
    | Object.Return value -> value
    | Object.Error message -> Object.Error message
    | result -> go result t in
  go Object.Null program

and eval_block_statement block_statement =
  let rec go last_result = function
    | [] -> last_result
    | h :: t ->
    match eval_statement h with
    | Object.Return value -> Object.Return value
    | Object.Error message -> Object.Error message
    | result -> go result t in
  go Object.Null block_statement
