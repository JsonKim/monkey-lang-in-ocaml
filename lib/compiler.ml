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

module CompilationScope = struct
  type t = {
    instructions : Code.instructions;
    last_instruction : EmittedInstruction.t option;
    previous_instruction : EmittedInstruction.t option;
  }
end

module Compiler = struct
  type t = {
    constants : Object.t array;
    symbol_table : Symbol_table.t;
    scopes : CompilationScope.t array;
    scope_index : int;
  }

  let make_scope () =
    CompilationScope.
      {
        instructions = Bytes.empty;
        last_instruction = None;
        previous_instruction = None;
      }

  let make () =
    {
      constants = [||];
      symbol_table = Symbol_table.empty;
      scopes = [|make_scope ()|];
      scope_index = 0;
    }

  let current_instructions c = c.scopes.(c.scope_index).instructions

  let enter_scope c =
    let scope = make_scope () in
    let scope_index = c.scope_index + 1 in
    let scopes = Array.append c.scopes [|scope|] in
    let symbol_table = Symbol_table.make_encloed_symbol_table c.symbol_table in
    { c with scopes; scope_index; symbol_table }

  exception Outer_Symbol_Table_Is_None

  let leave_scope c =
    let scopes = Array.sub c.scopes 0 ((c.scopes |> Array.length) - 1) in
    let scope_index = c.scope_index - 1 in
    (* 원래 구현에서는 pop된 instructions를 리턴 하고 있음 *)
    let symbol_table =
      match c.symbol_table.outer with
      | Some outer -> outer
      | None -> raise Outer_Symbol_Table_Is_None in

    { c with scopes; scope_index; symbol_table }

  let get_current_scope_and_leave_scope c =
    let instructions = current_instructions c in
    (instructions, leave_scope c)

  let make_with_state symbol_table constants =
    { (make ()) with symbol_table; constants }

  let add_constant c obj =
    let pos = Array.length c.constants in
    let constants = Array.append c.constants [|obj|] in
    ({ c with constants }, pos)

  let add_instruction c ins =
    let pos = c |> current_instructions |> Bytes.length in
    let updated_instructions = Bytes.cat (c |> current_instructions) ins in
    c.scopes.(c.scope_index) <-
      { (c.scopes.(c.scope_index)) with instructions = updated_instructions };
    (c, pos)

  let set_last_instruction op_code position c =
    let previous_instruction = c.scopes.(c.scope_index).last_instruction in
    let last_instruction = Some { EmittedInstruction.op_code; position } in
    c.scopes.(c.scope_index) <-
      { (c.scopes.(c.scope_index)) with previous_instruction; last_instruction };
    c

  let last_instruction_is op c =
    c.scopes.(c.scope_index).last_instruction
    |> Option.map (function
         | { EmittedInstruction.op_code; _ } when op_code = op -> true
         | _ -> false)
    |> Option.value ~default:false

  let remove_last_pop c =
    let last_position =
      c.scopes.(c.scope_index).last_instruction
      |> Option.map (fun ei -> ei.EmittedInstruction.position)
      |> Option.get in
    let instructions = current_instructions c in
    let instructions = Bytes.sub instructions 0 last_position in
    let last_instruction = c.scopes.(c.scope_index).previous_instruction in
    c.scopes.(c.scope_index) <-
      { (c.scopes.(c.scope_index)) with instructions; last_instruction };
    c

  let replace_instruction c pos new_instruction =
    Bytes.blit new_instruction 0
      (c |> current_instructions)
      pos
      (Bytes.length new_instruction)

  let change_operands c op_pos operand =
    let op =
      Bytes.get (c |> current_instructions) op_pos
      |> int_of_char
      |> Code.OpCode.to_op in
    let new_instruction = Code.make op operand in
    replace_instruction c op_pos new_instruction

  let replace_last_pop_with_return c =
    let last_pos =
      c.scopes.(c.scope_index).last_instruction
      |> Option.map (fun ei -> ei.EmittedInstruction.position)
      |> Option.get in
    replace_instruction c last_pos (Code.make OpReturnValue []);

    let scope = c.scopes.(c.scope_index) in
    let last_instruction = scope.last_instruction in
    let last_instruction =
      Option.map
        (fun ei -> { ei with EmittedInstruction.op_code = OpReturnValue })
        last_instruction in
    c.scopes.(c.scope_index) <- { scope with last_instruction }

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
      let bind_location =
        match symbol.scope with
        | Symbol_table.Symbol_scope.GLOBAL -> Code.OpCode.OpSetGlobal
        | Symbol_table.Symbol_scope.LOCAL -> Code.OpCode.OpSetLocal in
      let c = { c with symbol_table } in
      Ok (emit c bind_location [symbol.index])
    | Ast.ReturnStatement { value } ->
      let open Bindings.Result in
      let+ c, _ = compile_expression c value in
      emit c OpReturnValue []

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
    | Ast.Literal (Ast.Hash pairs) ->
      let open Bindings.Result in
      let* c, _ =
        List.fold_left
          (fun c (key, value) ->
            let* c, _ = c in
            let* c, _ = compile_expression c key in
            compile_expression c value)
          (Ok (c, 0))
          pairs in
      Ok (emit c OpHash [List.length pairs * 2])
    | Ast.If { condition; consequence; alternative } ->
      let open Bindings.Result in
      let* c, _ = compile_expression c condition in
      let c, jump_not_truthy_pos = emit c OpJumpNotTruthy [9999] in

      let* c, _ = compile_statements c consequence in
      let c = if last_instruction_is OpPop c then remove_last_pop c else c in

      let c, jump_pos = emit c OpJump [9999] in
      let after_consequense_pos = Bytes.length (c |> current_instructions) in
      change_operands c jump_not_truthy_pos [after_consequense_pos];

      let alternative =
        if alternative |> Option.is_none then
          Ok (emit c OpNull [])
        else
          let* c, pos = compile_statements c (Option.get alternative) in
          let c =
            if last_instruction_is OpPop c then
              remove_last_pop c
            else
              c in
          Ok (c, pos) in

      let+ c, pos = alternative in
      let after_alternative_pos = Bytes.length (c |> current_instructions) in
      change_operands c jump_pos [after_alternative_pos];
      (c, pos)
    | Ast.Identifier identifier -> (
      let symbol = Symbol_table.resolve identifier c.symbol_table in
      match symbol with
      | Some symbol ->
        let bind_location =
          match symbol.scope with
          | Symbol_table.Symbol_scope.GLOBAL -> Code.OpCode.OpGetGlobal
          | Symbol_table.Symbol_scope.LOCAL -> Code.OpCode.OpGetLocal in
        Ok (emit c bind_location [symbol.index])
      | None -> Error (Printf.sprintf "undefined variable %s" identifier))
    | Ast.Index { left; index } ->
      let open Bindings.Result in
      let* c, _ = compile_expression c left in
      let+ c, _ = compile_expression c index in
      emit c OpIndex []
    | Ast.Function { parameters = _; body } ->
      let open Bindings.Result in
      let c = enter_scope c in

      let+ c, _ = compile_statements c body in

      if last_instruction_is OpPop c then
        replace_last_pop_with_return c;

      let c =
        if last_instruction_is OpReturnValue c then
          c
        else
          emit c OpReturn [] |> fst in

      let instructions, c = get_current_scope_and_leave_scope c in
      let compiled_fn = Object.CompiledFunction instructions in
      let c, constant_index = add_constant c compiled_fn in

      emit c OpConstant [constant_index]
    | Ast.Call { fn; arguments = _ } ->
      let open Bindings.Result in
      let+ c, _ = compile_expression c fn in
      emit c OpCall []
    | _ -> raise Not_Implemented

  let compile c node =
    match node with
    | Ast.Program program -> compile_statements c program
    | Ast.Expression expression -> compile_expression c expression
    | Ast.Statement statement -> compile_statement c statement
    | _ -> raise Not_Implemented

  let to_bytecode c =
    {
      Bytecode.instructions = c |> current_instructions;
      constants = c.constants;
    }
end
