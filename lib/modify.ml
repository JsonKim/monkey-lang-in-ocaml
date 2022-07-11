open Ast

exception Not_Statement
exception Not_Expression

let node_to_statment = function
  | Statement statement -> statement
  | _ -> raise Not_Statement

let node_to_expression = function
  | Expression expression -> expression
  | _ -> raise Not_Expression

let node_to_statements = function
  | Program statements -> statements
  | BlockStatement statements -> statements
  | _ -> raise Not_Expression

let rec modify (modifier : node -> node) (node : node) =
  let traverse =
    List.map (fun stmt -> Statement stmt |> modify modifier |> node_to_statment)
  in
  match node with
  | Program statements -> Program (statements |> traverse)
  | BlockStatement statements -> BlockStatement (statements |> traverse)
  | Statement (ExpressionStatement { expression }) ->
    Statement
      (ExpressionStatement
         { expression = expression |> modify_expression modifier })
  | Statement (ReturnStatement { value }) ->
    Statement (ReturnStatement { value = value |> modify_expression modifier })
  | Statement (LetStatement stmt) ->
    Statement
      (LetStatement
         { stmt with value = stmt.value |> modify_expression modifier })
  | Expression (Infix expression) ->
    Expression
      (Infix
         {
           expression with
           left = expression.left |> modify_expression modifier;
           right = expression.right |> modify_expression modifier;
         })
  | Expression (Prefix expression) ->
    Expression
      (Prefix
         {
           expression with
           right = expression.right |> modify_expression modifier;
         })
  | Expression (Index { left; index }) ->
    Expression
      (Index
         {
           left = left |> modify_expression modifier;
           index = index |> modify_expression modifier;
         })
  | Expression (If { condition; consequence; alternative }) ->
    Expression
      (If
         {
           condition = condition |> modify_expression modifier;
           consequence =
             BlockStatement consequence |> modify modifier |> node_to_statements;
           alternative =
             Option.map
               (fun stmts ->
                 BlockStatement stmts |> modify modifier |> node_to_statements)
               alternative;
         })
  | Expression (Function expression) ->
    Expression
      (Function
         {
           expression with
           body =
             BlockStatement expression.body
             |> modify modifier
             |> node_to_statements;
         })
  | Expression (Literal (Array arr)) ->
    Expression (Literal (Array (arr |> List.map (modify_expression modifier))))
  | Expression (Literal (Hash pairs)) ->
    Expression
      (Literal
         (Hash
            (pairs
            |> List.map (fun (key, value) ->
                   ( modify_expression modifier key,
                     modify_expression modifier value )))))
  | Expression (Identifier _ | Literal _ | Call _) -> node |> modifier

and modify_expression modifier expression =
  Expression expression |> modify modifier |> node_to_expression
