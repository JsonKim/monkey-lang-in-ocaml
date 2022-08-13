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
    symbol_table : Symbol_table.t;
  }

  let empty =
    {
      instructions = Bytes.empty;
      constants = [||];
      last_instruction = None;
      previous_instruction = None;
      symbol_table = Symbol_table.empty;
    }

  let make_with_state symbol_table constants =
    { empty with symbol_table; constants }

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
      let open Bindings.Result in
      let+ c, _ = compile_expression c expression in
      emit c OpPop []
    | Ast.LetStatement { identifier; value } ->
      let open Bindings.Result in
      let* c, _ = compile_expression c value in
      let symbol, symbol_table = Symbol_table.define identifier c.symbol_table in
      let c = { c with symbol_table } in
      Ok (emit c Code.OpCode.OpSetGlobal [symbol.index])
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
    | Ast.Infix { left; right; token } -> (
      let open Bindings.Result in
      if token = Token.LT then
        let* c, _ = compile_expression c right in
        let+ c, _ = compile_expression c left in
        emit c OpGreaterThan []
      else
        let* c, _ = compile_expression c left in
        let* c, _ = compile_expression c right in
        match token with
        | Token.Plus -> Ok (emit c OpAdd [])
        | Token.Minus -> Ok (emit c OpSub [])
        | Token.Asterisk -> Ok (emit c OpMul [])
        | Token.Slash -> Ok (emit c OpDiv [])
        | Token.Eq -> Ok (emit c OpEqual [])
        | Token.Not_Eq -> Ok (emit c OpNotEqual [])
        | Token.GT -> Ok (emit c OpGreaterThan [])
        | token ->
          Error (Printf.sprintf "unknown operator %s" (token |> Token.show)))
    | Ast.Literal (Ast.Integer n) ->
      let integer = Object.Integer n in
      let c, pos = add_constant c integer in
      Ok (emit c Code.OpCode.OpConstant [pos])
    | Ast.Literal (Ast.String value) ->
      let str = Object.String value in
      let c, pos = add_constant c str in
      Ok (emit c Code.OpCode.OpConstant [pos])
    | Ast.Literal (Ast.Boolean true) -> Ok (emit c Code.OpCode.OpTrue [])
    | Ast.Literal (Ast.Boolean false) -> Ok (emit c Code.OpCode.OpFalse [])
    | Ast.Literal (Ast.Array values) ->
      let open Bindings.Result in
      let* c, _ =
        List.fold_left
          (fun c value ->
            let* c, _ = c in
            compile_expression c value)
          (Ok (c, 0))
          values in
      Ok (emit c OpArray [List.length values])
    | Ast.If { condition; consequence; alternative } ->
      let open Bindings.Result in
      let* c, _ = compile_expression c condition in
      let c, jump_not_truthy_pos = emit c OpJumpNotTruthy [9999] in

      let* c, _ = compile_statements c consequence in
      let c = if last_instruction_is_pop c then remove_last_pop c else c in

      let c, jump_pos = emit c OpJump [9999] in
      let after_consequense_pos = Bytes.length c.instructions in
      change_operands c jump_not_truthy_pos [after_consequense_pos];

      let alternative =
        if alternative |> Option.is_none then
          Ok (emit c OpNull [])
        else
          let* c, pos = compile_statements c (Option.get alternative) in
          let c =
            if last_instruction_is_pop c then
              remove_last_pop c
            else
              c in
          Ok (c, pos) in

      let+ c, pos = alternative in
      let after_alternative_pos = Bytes.length c.instructions in
      change_operands c jump_pos [after_alternative_pos];
      (c, pos)
    | Ast.Identifier identifier -> (
      let symbol = Symbol_table.resolve identifier c.symbol_table in
      match symbol with
      | Some symbol -> Ok (emit c OpGetGlobal [symbol.index])
      | None -> Error (Printf.sprintf "undefined variable %s" identifier))
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
