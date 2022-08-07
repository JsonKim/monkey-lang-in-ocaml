exception Not_Implemented

module Bytecode = struct
  type t = {
    instructions : Code.instructions;
    constants : Object.t array;
  }
end

module EmittedInstruction = struct
  type t = {
    op_code : Code.OpCode.t;
    position : int;
  }
end

module Compiler = struct
  type t = {
    instructions : Code.instructions;
    constants : Object.t array;
    last_instruction : EmittedInstruction.t option;
    previous_instruction : EmittedInstruction.t option;
  }

  let empty =
    {
      instructions = Bytes.empty;
      constants = [||];
      last_instruction = None;
      previous_instruction = None;
    }

  let add_constant c obj =
    let pos = Array.length c.constants in
    let constants = Array.append c.constants [|obj|] in
    ({ c with constants }, pos)

  let add_instruction c ins =
    let pos = Bytes.length c.instructions in
    let instructions = Bytes.cat c.instructions ins in
    ({ c with instructions }, pos)

  let set_last_instruction op_code position c =
    let previous_instruction = c.last_instruction in
    let last_instruction = Some { EmittedInstruction.op_code; position } in
    { c with previous_instruction; last_instruction }

  let last_instruction_is_pop c =
    c.last_instruction
    |> Option.map (function
         | { EmittedInstruction.op_code = Code.OpCode.OpPop; _ } -> true
         | _ -> false)
    |> Option.value ~default:false

  let remove_last_pop c =
    let instructions =
      Bytes.sub c.instructions 0 (Bytes.length c.instructions - 1) in
    let last_instruction = c.previous_instruction in
    { c with instructions; last_instruction }

  let replace_instruction c pos new_instruction =
    Bytes.blit new_instruction 0 c.instructions pos
      (Bytes.length new_instruction)

  let change_operands c op_pos operand =
    let op =
      Bytes.get c.instructions op_pos |> int_of_char |> Code.OpCode.to_op in
    let new_instruction = Code.make op operand in
    replace_instruction c op_pos new_instruction

  let emit c op operands =
    let ins = Code.make op operands in
    let c, pos = add_instruction c ins in
    let c = set_last_instruction op pos c in
    (c, pos)

  let rec compile_statement c stmt =
    match stmt with
    | Ast.ExpressionStatement { expression } ->
      compile_expression c expression
      |> Result.map (fun (c, _) -> emit c OpPop [])
    | _ -> raise Not_Implemented

  and compile_statements c stmts =
    List.fold_left
      (fun result stmt ->
        Result.bind result (fun (c, _) -> compile_statement c stmt))
      (Ok (c, 0))
      stmts

  and compile_expression c expression =
    match expression with
    | Ast.Prefix { token; right } ->
      Result.bind (compile_expression c right) (fun (c, _) ->
          match token with
          | Token.Minus -> Ok (emit c OpMinus [])
          | Token.Bang -> Ok (emit c OpBang [])
          | _ ->
            Error (Printf.sprintf "unknown operator %s" (token |> Token.show)))
    | Ast.Infix { left; right; token } ->
      if token = Token.LT then
        Result.bind (compile_expression c right) (fun (c, _) ->
            Result.bind (compile_expression c left) (fun (c, _) ->
                Ok (emit c OpGreaterThan [])))
      else
        Result.bind (compile_expression c left) (fun (c, _) ->
            Result.bind (compile_expression c right) (fun (c, _) ->
                match token with
                | Token.Plus -> Ok (emit c OpAdd [])
                | Token.Minus -> Ok (emit c OpSub [])
                | Token.Asterisk -> Ok (emit c OpMul [])
                | Token.Slash -> Ok (emit c OpDiv [])
                | Token.Eq -> Ok (emit c OpEqual [])
                | Token.Not_Eq -> Ok (emit c OpNotEqual [])
                | Token.GT -> Ok (emit c OpGreaterThan [])
                | token ->
                  Error
                    (Printf.sprintf "unknown operator %s" (token |> Token.show))))
    | Ast.Literal (Ast.Integer n) ->
      let integer = Object.Integer n in
      let c, pos = add_constant c integer in
      Ok (emit c Code.OpCode.OpConstant [pos])
    | Ast.Literal (Ast.Boolean true) -> Ok (emit c Code.OpCode.OpTrue [])
    | Ast.Literal (Ast.Boolean false) -> Ok (emit c Code.OpCode.OpFalse [])
    | Ast.If { condition; consequence; alternative } ->
      Result.bind (compile_expression c condition) (fun (c, _) ->
          let c, jump_not_truthy_pos = emit c OpJumpNotTruthy [9999] in
          Result.bind (compile_statements c consequence) (fun (c, pos) ->
              let c =
                if last_instruction_is_pop c then remove_last_pop c else c in
              if alternative |> Option.is_none then (
                let after_consequense_pos = Bytes.length c.instructions in
                change_operands c jump_not_truthy_pos [after_consequense_pos];
                Ok (c, pos))
              else
                let c, jump_pos = emit c OpJump [9999] in
                let after_consequense_pos = Bytes.length c.instructions in
                change_operands c jump_not_truthy_pos [after_consequense_pos];
                Result.bind
                  (compile_statements c (Option.get alternative))
                  (fun (c, pos) ->
                    let c =
                      if last_instruction_is_pop c then remove_last_pop c else c
                    in
                    let after_alternative_pos = Bytes.length c.instructions in
                    change_operands c jump_pos [after_alternative_pos];
                    Ok (c, pos))))
    | _ -> raise Not_Implemented

  let compile c node =
    match node with
    | Ast.Program program -> compile_statements c program
    | Ast.Expression expression -> compile_expression c expression
    | Ast.Statement statement -> compile_statement c statement
    | _ -> raise Not_Implemented

  let to_bytecode c =
    { Bytecode.instructions = c.instructions; constants = c.constants }
end
