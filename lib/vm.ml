exception Not_Converted

let stack_size = 2048

type t = {
  constants : Object.t array;
  instructions : Code.instructions;
  stack : Object.t array;
  sp : int;
}

let make bytecode =
  let open Compiler.Bytecode in
  let { instructions; constants } = bytecode in
  let stack = Array.make stack_size Object.Null in
  { instructions; constants; stack; sp = 0 }

let last_popped_stack_elem vm =
  try Some (Array.get vm.stack vm.sp) with
  | _ -> None

let push o vm =
  if vm.sp >= stack_size then
    raise Stack_overflow
  else (
    Array.set vm.stack vm.sp o;
    { vm with sp = vm.sp + 1 })

let pop vm =
  let obj = Array.get !vm.stack (!vm.sp - 1) in
  vm := { !vm with sp = !vm.sp - 1 };
  obj

let run vm =
  let ip = ref 0 in
  let vm = ref vm in
  let size_of_instructions = Bytes.length !vm.instructions in
  while !ip < size_of_instructions do
    let op =
      Bytes.get !vm.instructions !ip |> int_of_char |> Code.OpCode.to_op in
    (match op with
    | Code.OpCode.OpConstant ->
      let const_index = Bytes.get_uint16_be !vm.instructions (!ip + 1) in
      vm := push (Array.get !vm.constants const_index) !vm;
      ip := !ip + 2
    | OpAdd ->
      let object_to_integer = function
        | Object.Integer n -> n
        | _ -> raise Not_Converted in
      let right = pop vm in
      let left = pop vm in
      let leftValue = left |> object_to_integer in
      let rightValue = right |> object_to_integer in
      let result = leftValue + rightValue in
      vm := push (Object.Integer result) !vm
    | OpPop -> pop vm |> ignore);
    ip := !ip + 1
  done;
  !vm
