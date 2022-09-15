type instructions = bytes [@@deriving show, eq]
type byte = char

module OpCode = struct
  exception Not_OpCode

  type t =
    | OpConstant
    | OpPop
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpTrue
    | OpFalse
    | OpEqual
    | OpNotEqual
    | OpGreaterThan
    | OpMinus
    | OpBang
    | OpJumpNotTruthy
    | OpJump
    | OpNull
    | OpGetGlobal
    | OpSetGlobal
    | OpArray
    | OpHash
    | OpIndex
    | OpCall
    | OpReturnValue
    | OpReturn
    | OpGetLocal
    | OpSetLocal
  [@@deriving show { with_path = false }]

  let to_int = function
    | OpConstant -> 0
    | OpPop -> 1
    | OpAdd -> 2
    | OpSub -> 3
    | OpMul -> 4
    | OpDiv -> 5
    | OpTrue -> 6
    | OpFalse -> 7
    | OpEqual -> 8
    | OpNotEqual -> 9
    | OpGreaterThan -> 10
    | OpMinus -> 11
    | OpBang -> 12
    | OpJumpNotTruthy -> 13
    | OpJump -> 14
    | OpNull -> 15
    | OpGetGlobal -> 16
    | OpSetGlobal -> 17
    | OpArray -> 18
    | OpHash -> 19
    | OpIndex -> 20
    | OpCall -> 21
    | OpReturnValue -> 22
    | OpReturn -> 23
    | OpGetLocal -> 24
    | OpSetLocal -> 25

  let to_op = function
    | 0 -> OpConstant
    | 1 -> OpPop
    | 2 -> OpAdd
    | 3 -> OpSub
    | 4 -> OpMul
    | 5 -> OpDiv
    | 6 -> OpTrue
    | 7 -> OpFalse
    | 8 -> OpEqual
    | 9 -> OpNotEqual
    | 10 -> OpGreaterThan
    | 11 -> OpMinus
    | 12 -> OpBang
    | 13 -> OpJumpNotTruthy
    | 14 -> OpJump
    | 15 -> OpNull
    | 16 -> OpGetGlobal
    | 17 -> OpSetGlobal
    | 18 -> OpArray
    | 19 -> OpHash
    | 20 -> OpIndex
    | 21 -> OpCall
    | 22 -> OpReturnValue
    | 23 -> OpReturn
    | 24 -> OpGetLocal
    | 25 -> OpSetLocal
    | _ -> raise Not_OpCode

  let to_byte op = op |> to_int |> char_of_int
  let compare a z = (a |> to_int) - (z |> to_int)
end

module Definitions = Map.Make (OpCode)

let definitions =
  OpCode.(
    function
    | OpConstant -> [2]
    | OpPop -> []
    | OpAdd -> []
    | OpSub -> []
    | OpMul -> []
    | OpDiv -> []
    | OpTrue -> []
    | OpFalse -> []
    | OpEqual -> []
    | OpNotEqual -> []
    | OpGreaterThan -> []
    | OpMinus -> []
    | OpBang -> []
    | OpJumpNotTruthy -> [2]
    | OpJump -> [2]
    | OpNull -> []
    | OpGetGlobal -> [2]
    | OpSetGlobal -> [2]
    | OpArray -> [2]
    | OpHash -> [2]
    | OpIndex -> []
    | OpCall -> [1]
    | OpReturnValue -> []
    | OpReturn -> []
    | OpGetLocal -> [1]
    | OpSetLocal -> [1])

let make op operands =
  let operand_widths = definitions op in
  let instruction_len = 1 + List.fold_left ( + ) 0 operand_widths in
  let instruction = Bytes.make instruction_len '\x00' in
  Bytes.set instruction 0 (op |> OpCode.to_byte);
  List.fold_left
    (fun offset (operand, width) ->
      (match width with
      | 2 -> Bytes.set_int16_be instruction offset operand
      | 1 -> Bytes.set_int8 instruction offset operand
      | _ -> ());
      offset + width)
    1
    (List.combine operands operand_widths)
  |> ignore;
  instruction

let read_uint_8 ins = Bytes.get_uint8 ins 0
let read_uint_16 ins = Bytes.get_uint16_be ins 0

exception Invalid_Operand_Width

let read_operands operands_width ins =
  List.fold_left
    (fun (operands, offset) width ->
      let arg =
        match width with
        | 2 -> ins |> read_uint_16
        | 1 -> ins |> read_uint_8
        | _ -> raise Invalid_Operand_Width in
      (Array.append operands [|arg|], offset + width))
    (Array.make 0 0, 0)
    operands_width

let instruction_to_string offset ins =
  let op = Bytes.get ins 0 |> int_of_char |> OpCode.to_op in
  let def = definitions op in
  let operands, read_bytes =
    read_operands def (Bytes.sub ins 1 ((ins |> Bytes.length) - 1)) in
  let prefix = Printf.sprintf "%04d" offset in
  let arguments =
    match List.length def with
    | 0 -> op |> OpCode.show
    | 1 -> Printf.sprintf "%s %d" (op |> OpCode.show) operands.(0)
    | _ -> "" in
  (Printf.sprintf "%s %s" prefix arguments, 1 + read_bytes)

let to_string ins =
  let length_of_ins = ins |> Bytes.length in
  let rec loop offset str =
    if offset < length_of_ins then
      let ins = Bytes.sub ins offset (length_of_ins - offset) in
      let s, read_bytes = instruction_to_string offset ins in
      let str =
        match str with
        | "" -> ""
        | _ -> str ^ "\n" in
      loop (offset + read_bytes) (str ^ s)
    else
      str in

  loop 0 ""
