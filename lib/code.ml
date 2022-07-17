type instructions = bytes
type byte = char

module OpCode = struct
  type t = OpConstant

  let to_int = function
    | OpConstant -> 0

  let to_byte op = op |> to_int |> char_of_int
  let compare a z = (a |> to_int) - (z |> to_int)
end

module Definitions = Map.Make (OpCode)

let definitions =
  OpCode.(
    function
    | OpConstant -> [2])

let make op operands =
  let operand_widths = definitions op in
  let instruction_len = 1 + List.fold_left ( + ) 0 operand_widths in
  let instruction = Bytes.make instruction_len '\x00' in
  Bytes.set instruction 0 (OpCode.OpConstant |> OpCode.to_byte);
  List.fold_left
    (fun offset (operand, width) ->
      (match width with
      | 2 -> Bytes.set_int16_be instruction offset operand
      | _ -> ());
      offset + width)
    1
    (List.combine operands operand_widths)
  |> ignore;
  instruction
