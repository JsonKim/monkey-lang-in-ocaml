exception Not_Converted

let stack_size = 2048
let global_size = 65535
let max_frames = 1024
let obj_true = Object.Boolean true
let obj_false = Object.Boolean false
let native_bool_to_boolean_object b = if b then obj_true else obj_false

let is_truthy = function
  | Object.Boolean b -> b
  | Object.Null -> false
  | _ -> true

type t = {
  constants : Object.t array;
  stack : Object.t array;
  sp : int;
  globals : Object.t array;
  frames : Frame.t array;
  frames_index : int;
}

let empty_globals = Array.make global_size Object.Null

let make bytecode =
  let open Compiler.Bytecode in
  let { instructions; constants } = bytecode in
  let main_fn = instructions in
  let main_frame = Frame.make main_fn in

  let frames = Array.make max_frames (Frame.make Bytes.empty) in
  frames.(0) <- main_frame;

  let stack = Array.make stack_size Object.Null in
  {
    constants;
    stack;
    sp = 0;
    globals = empty_globals;
    frames;
    frames_index = 1;
  }

let make_with_globals_store globals bytecode = { (make bytecode) with globals }

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

let current_frame vm = vm.frames.(vm.frames_index - 1)

let push_frame vm f =
  vm.frames.(vm.frames_index) <- f;
  { vm with frames_index = vm.frames_index + 1 }

let pop_frame vm =
  let frames_index = vm.frames_index - 1 in
  ({ vm with frames_index }, vm.frames.(vm.frames_index))

let execute_binary_integer_operation vm op l r =
  let open Code.OpCode in
  let result =
    match op with
    | OpAdd -> l + r
    | OpSub -> l - r
    | OpMul -> l * r
    | OpDiv -> l / r
    | _ -> raise Not_Converted in
  vm := push (Object.Integer result) !vm

let execute_binary_string_operation vm op l r =
  let open Code.OpCode in
  let result =
    match op with
    | OpAdd -> l ^ r
    | _ -> raise Not_Converted in
  vm := push (Object.String result) !vm

let execute_binary_operation vm op =
  let right = pop vm in
  let left = pop vm in
  match (left, right) with
  | Object.Integer l, Object.Integer r ->
    execute_binary_integer_operation vm op l r
  | Object.String l, Object.String r ->
    execute_binary_string_operation vm op l r
  | _ -> raise Not_Converted

let execute_integer_comparison vm op l r =
  let open Code.OpCode in
  let result =
    match op with
    | OpEqual -> l = r
    | OpNotEqual -> l != r
    | OpGreaterThan -> l > r
    | _ -> raise Not_Converted in
  vm := push (result |> native_bool_to_boolean_object) !vm

let execute_comparison vm op =
  let right = pop vm in
  let left = pop vm in
  match (left, right) with
  | Object.Integer l, Object.Integer r -> execute_integer_comparison vm op l r
  | Object.Boolean l, Object.Boolean r ->
    let result =
      match op with
      | OpEqual -> l = r
      | OpNotEqual -> l != r
      | _ -> raise Not_Converted in
    vm := push (result |> native_bool_to_boolean_object) !vm
  | _ -> raise Not_Converted

let execute_minus_operator vm =
  let operand = pop vm in
  match operand with
  | Object.Integer n -> vm := push (Object.Integer (-n)) !vm
  | _ -> raise Not_Converted

let execute_bang_operator vm =
  let operand = pop vm in
  match operand with
  | Object.Boolean true -> vm := push obj_false !vm
  | Object.Boolean false -> vm := push obj_true !vm
  | Object.Null -> vm := push obj_true !vm
  | _ -> vm := push obj_false !vm

let execute_array_index vm array index =
  if index < 0 || index >= List.length array then
    vm := push Object.Null !vm
  else
    vm := push (List.nth array index) !vm

let execute_hash_index vm hash index =
  let value =
    if Object.is_hashable index then
      match hash |> Object.Hash.find_opt (Object.show index) with
      | Some { Object.key = _; value } -> value
      | None -> Object.Null
    else
      Object.Null in
  vm := push value !vm

let execute_index_expression vm left index =
  match (left, index) with
  | Object.Array array, Object.Integer index ->
    execute_array_index vm array index
  | Object.Hash hash, _ -> execute_hash_index vm hash index
  | _ -> raise Not_Converted

let build_array strat_index end_index vm =
  let elements = Array.sub vm.stack strat_index (end_index - strat_index) in
  Object.Array (elements |> Array.to_list)

let build_hash strat_index end_index vm =
  let hash = ref Object.empty_hash in
  let index = ref strat_index in
  while !index < end_index do
    let key = vm.stack.(!index) in
    let value = vm.stack.(!index + 1) in
    hash := !hash |> Object.add_hash key value;

    index := !index + 2
  done;
  Object.Hash !hash

let run vm =
  let move_current_frame_ip vm value =
    let frame = !vm |> current_frame in
    let frame = { frame with ip = frame.ip + value } in
    !vm.frames.(!vm.frames_index - 1) <- frame in

  let jump_current_frame_ip vm ip =
    let frame = !vm |> current_frame in
    let frame = { frame with ip } in
    !vm.frames.(!vm.frames_index - 1) <- frame in

  let vm = ref vm in
  let size_of_instructions vm =
    Bytes.length (!vm |> current_frame |> Frame.instructions) in
  while (!vm |> current_frame).ip < (vm |> size_of_instructions) - 1 do
    move_current_frame_ip vm 1;

    let ip = (!vm |> current_frame).ip in
    let instructions vm = !vm |> current_frame |> Frame.instructions in
    let op =
      Bytes.get (vm |> instructions) ip |> int_of_char |> Code.OpCode.to_op
    in
    match op with
    | Code.OpCode.OpConstant ->
      let const_index = Bytes.get_uint16_be (vm |> instructions) (ip + 1) in
      move_current_frame_ip vm 2;
      vm := push (Array.get !vm.constants const_index) !vm
    | OpPop -> pop vm |> ignore
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv ->
      execute_binary_operation vm op
    | OpTrue -> vm := push obj_true !vm
    | OpFalse -> vm := push obj_false !vm
    | OpEqual
    | OpNotEqual
    | OpGreaterThan ->
      execute_comparison vm op
    | OpMinus -> execute_minus_operator vm
    | OpBang -> execute_bang_operator vm
    | OpJumpNotTruthy ->
      let operand = Bytes.sub (vm |> instructions) (ip + 1) 2 in
      let pos = Code.read_uint_16 operand in
      move_current_frame_ip vm 2;

      let condition = pop vm in
      if condition |> is_truthy = false then
        jump_current_frame_ip vm (pos - 1)
    | OpJump ->
      let operand = Bytes.sub (vm |> instructions) (ip + 1) 2 in
      let pos = Code.read_uint_16 operand in
      jump_current_frame_ip vm (pos - 1)
    | OpNull -> vm := push Object.Null !vm
    | OpGetGlobal ->
      let operand = Bytes.sub (vm |> instructions) (ip + 1) 2 in
      let global_index = Code.read_uint_16 operand in
      move_current_frame_ip vm 2;
      vm := push !vm.globals.(global_index) !vm
    | OpSetGlobal ->
      let operand = Bytes.sub (vm |> instructions) (ip + 1) 2 in
      let global_index = Code.read_uint_16 operand in
      move_current_frame_ip vm 2;

      let global_value = pop vm in
      !vm.globals.(global_index) <- global_value
    | OpArray ->
      let operand = Bytes.sub (vm |> instructions) (ip + 1) 2 in
      let num_elements = Code.read_uint_16 operand in
      move_current_frame_ip vm 2;

      let array = build_array (!vm.sp - num_elements) !vm.sp !vm in
      vm := push array !vm
    | OpHash ->
      let operand = Bytes.sub (vm |> instructions) (ip + 1) 2 in
      let num_elements = Code.read_uint_16 operand in
      move_current_frame_ip vm 2;

      let hash = build_hash (!vm.sp - num_elements) !vm.sp !vm in
      vm := push hash !vm
    | OpIndex ->
      let index = pop vm in
      let left = pop vm in
      execute_index_expression vm left index
    | OpCall ->
      let fn = !vm.stack.(!vm.sp - 1) in
      let fn =
        match fn with
        | Object.CompiledFunction fn -> fn
        | _ -> raise Not_Converted in
      let frame = Frame.make fn in
      vm := push_frame !vm frame
    | OpReturnValue ->
      let return_value = pop vm in
      vm := !vm |> pop_frame |> fst;
      vm |> pop |> ignore;
      vm := push return_value !vm
    | OpReturn -> (* FIXME *) ()
  done;
  !vm
