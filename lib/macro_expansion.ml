open Ast

exception Not_Program
exception Not_Ast

let parse_macro_from_let = function
  | LetStatement { identifier; value = Literal (Macro macro) } ->
    Some (identifier, macro)
  | _ -> None

let add_macro (identifier, macro) env =
  Environment.set identifier
    (Object.Macro { parameters = macro.parameters; body = macro.body; env })
    env

let define_macros env node =
  let env, statements =
    match node with
    | Program program ->
      List.fold_left
        (fun (env, program) stmt ->
          match parse_macro_from_let stmt with
          | Some macro -> (add_macro macro env, program)
          | None -> (env, program @ [stmt]))
        (env, []) program
    | _ -> raise Not_Program in
  (env, Program statements)

let parse_macro_call env = function
  | Expression (Call { fn = Identifier identifier; arguments }) -> (
    match Environment.get identifier env with
    | Some (Object.Macro macro) -> Some (macro, arguments)
    | Some _ -> None
    | None -> None)
  | _ -> None

let quote_args arguments =
  List.fold_right (fun exp args -> Object.Quote exp :: args) arguments []

let extend_macro_env Object.{ env; parameters; _ } args =
  let extended = env |> Environment.make_enclosed in
  let pairs = List.combine parameters args in
  let extended =
    List.fold_right
      (fun (key, value) env -> Environment.set key value env)
      pairs extended in
  extended

let expand_macros env node =
  Modify.modify
    (fun node ->
      match parse_macro_call env node with
      | Some (macro, arguments) -> (
        let args = quote_args arguments in
        let eval_env = extend_macro_env macro args in
        match Evaluator.eval_block_statement eval_env macro.body with
        | Object.Quote expression, _ -> Ast.Expression expression
        | _ -> raise Not_Ast)
      | None -> node)
    node
