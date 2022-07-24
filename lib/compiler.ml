exception Not_Implemented

module Bytecode = struct
  type t = {
    instructions : Code.instructions;
    constants : Object.t array;
  }
end

module Compiler = struct
  type t = {
    instructions : Code.instructions;
    constants : Object.t array;
  }
  [@@deriving show, eq]

  let empty = { instructions = Bytes.empty; constants = [||] }

  let add_constant c obj =
    let pos = Array.length c.constants in
    let constants = Array.append c.constants [|obj|] in
    ({ c with constants }, pos)

  let add_instruction c ins =
    let pos = Bytes.length c.instructions in
    let instructions = Bytes.cat c.instructions ins in
    ({ c with instructions }, pos)

  let emit c op operands =
    let ins = Code.make op operands in
    add_instruction c ins

  let rec compile_statement c stmt =
    match stmt with
    | Ast.ExpressionStatement { expression } -> compile_expression c expression
    | _ -> raise Not_Implemented

  and compile_statements c stmts =
    List.fold_left
      (fun result stmt ->
        Option.bind result (fun (c, _) -> compile_statement c stmt))
      (Some (c, 0))
      stmts

  and compile_expression c expression =
    match expression with
    | Ast.Infix { left; right; _ } ->
      Option.bind (compile_expression c left) (fun (c, _) ->
          compile_expression c right)
    | Ast.Literal (Ast.Integer n) ->
      let integer = Object.Integer n in
      let c, pos = add_constant c integer in
      Some (emit c Code.OpCode.OpConstant [pos])
    | _ -> raise Not_Implemented

  let compile c node =
    match node with
    | Ast.Program program -> compile_statements c program
    | Ast.Expression expression -> compile_expression c expression
    | _ -> raise Not_Implemented
end
