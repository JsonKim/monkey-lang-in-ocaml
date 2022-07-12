open Ast

exception Not_Convert

let trueObject = Object.Boolean true
let falseObject = Object.Boolean false

let is_truthy = function
  | Object.Null -> false
  | Object.Boolean b -> b
  | _ -> true

let rec eval_statement env node =
  match node with
  | ExpressionStatement { expression } -> eval_expression env expression
  | ReturnStatement { value } -> (
    match eval_expression env value with
    | Object.Error message, env -> (Object.Error message, env)
    | value, env -> (Object.Return value, env))
  | LetStatement { identifier; value } ->
  match eval_expression env value with
  | Object.Error message, env -> (Object.Error message, env)
  | value, env -> (Object.Null, Environment.set identifier value env)

and eval_expression env = function
  | Literal (Integer n) -> (Object.Integer n, env)
  | Literal (Boolean true) -> (trueObject, env)
  | Literal (Boolean false) -> (falseObject, env)
  | Literal (String str) -> (Object.String str, env)
  | Literal (Array arr) -> (
    let fn (acc, env) ele =
      let ele, env = eval_expression env ele in
      (acc @ [ele], env) in
    match List.fold_left fn ([], env) arr with
    | acc, env -> (Object.Array acc, env))
  | Literal (Hash hash) -> (
    let fn (acc, env) (key, value) =
      match acc with
      | Object.Hash acc -> (
        match eval_expression env key with
        | key, env when Object.is_hashable key ->
          let value, env = eval_expression env value in
          (Object.Hash (Object.add_hash key value acc), env)
        | key, env ->
          ( Object.Error ("unusable as hash key: " ^ Object.decode_tag_of key),
            env ))
      | _ -> (Object.Error "not reach here", env) in

    match List.fold_left fn (Object.Hash Object.empty_hash, env) hash with
    | acc, env -> (acc, env))
  | Prefix { token; right } -> (
    match eval_expression env right with
    | Object.Error message, env -> (Object.Error message, env)
    | right, env -> (eval_prefix token right, env))
  | Infix { token; left; right } -> (
    match eval_expression env left with
    | Object.Error message, env -> (Object.Error message, env)
    | left, env ->
    match eval_expression env right with
    | Object.Error message, env -> (Object.Error message, env)
    | right, env -> (eval_infix token left right, env))
  | If { condition; consequence; alternative } -> (
    match condition |> eval_expression env with
    | Object.Error message, env -> (Object.Error message, env)
    | condition, env -> (
      if condition |> is_truthy then
        eval_block_statement env consequence
      else
        match alternative with
        | Some alternative -> eval_block_statement env alternative
        | None -> (Object.Null, env)))
  | Identifier ident -> (
    match Environment.get ident env with
    | Some obj -> (obj, env)
    | None -> (
      let open Builtins in
      match Builtin.find_opt ident fns with
      | Some (Object.Builtin { fn }) -> (Object.Builtin { fn }, env)
      | _ -> (Object.Error ("identifier not found: " ^ ident), env)))
  | Function { parameters; body } ->
    (Object.Function { parameters; body; env }, env)
  | Call { fn; arguments } -> (
    match fn with
    | Ast.Identifier "quote" -> quote env arguments
    | _ ->
    match eval_expression env fn with
    | Object.Error message, env -> (Object.Error message, env)
    | fn, env ->
    match eval_expressions env arguments with
    | [], env -> (Object.Error "arguments is empty", env)
    | [Object.Error message], env -> (Object.Error message, env)
    | args, env -> (apply_function fn args, env))
  | Index { left; index } ->
  match eval_expression env left with
  | Object.Array arr, env -> (
    match eval_expression env index with
    | Object.Error message, env -> (Object.Error message, env)
    | index, env -> (eval_array_index arr index, env))
  | Object.Hash hash, env -> (
    match eval_expression env index with
    | Object.Error message, env -> (Object.Error message, env)
    | index, env -> (eval_hash_index hash index, env))
  | Object.Error message, env -> (Object.Error message, env)
  | obj, env ->
    ( Object.Error ("index operator not supported:" ^ Object.decode_tag_of obj),
      env )

and eval_expressions env args =
  let rec go env acc = function
    | [] -> (acc, env)
    | arg :: rest ->
    match eval_expression env arg with
    | Object.Error message, env -> ([Object.Error message], env)
    | obj, env -> go env (acc @ [obj]) rest in
  go env [] args

and apply_function fn args =
  match fn with
  | Object.Function { parameters; body; env } -> (
    let env = extend_function_env (parameters, args) env in
    match eval_block_statement env body |> fst with
    | Object.Return value -> value
    | obj -> obj)
  | Object.Builtin { fn } -> fn args
  | _ -> Object.Error ("not a function: " ^ Object.show fn)

and extend_function_env (parameters, args) env =
  let rec go env = function
    | [] -> env
    | (parameter, arg) :: rest ->
      let env = Environment.set parameter arg env in
      go env rest in

  let env = Environment.make_enclosed env in
  let p = List.combine parameters args in
  go env p

and eval_prefix token right =
  match token with
  | Token.Bang -> eval_bang right
  | Token.Minus -> eval_minus right
  | _ ->
    Object.Error ("unknown operator: " ^ Token.show token ^ Object.show right)

and eval_infix token left right =
  match (token, left, right) with
  | _, Integer l, Integer r -> eval_integer_infix token l r
  | _, String l, String r -> eval_string_infix token l r
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

and eval_string_infix token left right =
  match token with
  | Token.Plus -> Object.String (left ^ right)
  | _ ->
    Object.Error
      ("unknown operator: " ^ left ^ " " ^ Token.show token ^ " " ^ right)

and eval_bang = function
  | Object.Boolean true -> falseObject
  | Object.Boolean false -> trueObject
  | Object.Null -> trueObject
  | _ -> falseObject

and eval_minus right =
  match right with
  | Object.Integer n -> Object.Integer (-n)
  | _ -> Object.Error ("unknown operator: -" ^ Object.show right)

and eval_array_index arr index =
  match index with
  | Object.Integer n when n < 0 -> Object.Null
  | Object.Integer n -> (
    match List.nth_opt arr n with
    | Some ele -> ele
    | None -> Object.Null)
  | _ ->
    Object.Error ("index operator not supported: " ^ Object.decode_tag_of index)

and eval_hash_index hash index =
  if Object.is_hashable index then
    match hash |> Object.Hash.find_opt (Object.show index) with
    | Some { key = _; value } -> value
    | None -> Object.Null
  else
    Object.Error ("index operator not supported: " ^ Object.decode_tag_of index)

and eval env node =
  match node with
  | Ast.Program program -> eval_program env program
  | Ast.BlockStatement block_statement ->
    eval_block_statement env block_statement
  | Ast.Statement statement -> eval_statement env statement
  | Ast.Expression expression -> eval_expression env expression

and eval_program env program =
  let rec go (last_result, env) = function
    | [] -> (last_result, env)
    | h :: t ->
    match eval_statement env h with
    | Object.Return value, env -> (value, env)
    | Object.Error message, env -> (Object.Error message, env)
    | result, env -> go (result, env) t in
  go (Object.Null, env) program

and eval_block_statement env block_statement =
  let rec go (last_result, env) = function
    | [] -> (last_result, env)
    | h :: t ->
    match eval_statement env h with
    | Object.Return value, env -> (Object.Return value, env)
    | Object.Error message, env -> (Object.Error message, env)
    | result, env -> go (result, env) t in
  go (Object.Null, env) block_statement

and convert_object_to_ast_node = function
  | Object.Integer n -> n |> Ast.int_to_literal
  | Object.Boolean b -> b |> Ast.bool_to_literal
  | Object.Quote expression -> expression
  | _ -> raise Not_Convert

and quote env arguments =
  if List.length arguments == 1 then
    let arg = List.nth arguments 0 in
    let arg =
      Modify.modify_expression
        (function
          | Expression (Call { fn = Identifier "unquote"; arguments }) ->
            let obj, _ = eval_expression env (List.nth arguments 0) in
            Expression (obj |> convert_object_to_ast_node)
          | node -> node)
        arg in
    (Object.Quote arg, env)
  else
    ( Object.Error
        ("wrong number of arguments. got="
        ^ (arguments |> List.length |> string_of_int)
        ^ ", want=1"),
      env )
