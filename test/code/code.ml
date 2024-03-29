open Monkey

let to_bytes arr = arr |> Array.map char_of_int |> Array.to_seq |> Bytes.of_seq

let concat_bytes l =
  List.fold_right (fun b acc -> Bytes.cat b acc) l Bytes.empty

let test_make () =
  let open Alcotest in
  let open Code.OpCode in
  check (list bytes) "same object"
    [
      [|OpConstant |> to_int; 255; 254|] |> to_bytes;
      [|OpAdd |> to_int|] |> to_bytes;
      [|OpGetLocal |> to_int; 255|] |> to_bytes;
      [|OpClosure |> to_int; 255; 254; 255|] |> to_bytes;
    ]
    [
      Code.make OpConstant [65534];
      Code.make OpAdd [];
      Code.make OpGetLocal [255];
      Code.make OpClosure [65534; 255];
    ]

type read_operand = {
  op : Code.OpCode.t;
  operands : int array;
  bytes_read : int;
}

let test_read_operands () =
  let open Alcotest in
  let open Code.OpCode in
  let slice_second_to_last buf = Bytes.sub buf 1 ((buf |> Bytes.length) - 1) in
  let data =
    [
      { op = OpConstant; operands = [|65535|]; bytes_read = 2 };
      { op = OpGetLocal; operands = [|255|]; bytes_read = 1 };
    ] in
  check
    (list (pair (int |> array) int))
    "same object"
    (List.map
       (fun data ->
         let code = Code.make data.op (data.operands |> Array.to_list) in
         Code.read_operands (Code.definitions data.op)
           (code |> slice_second_to_last))
       data)
    (List.map (fun data -> (data.operands, data.bytes_read)) data)

let test_instructions_to_string () =
  let open Alcotest in
  let instructinos =
    [
      Code.make OpAdd [];
      Code.make OpGetLocal [1];
      Code.make OpConstant [2];
      Code.make OpConstant [65534];
      Code.make OpClosure [65535; 255];
    ]
    |> concat_bytes in
  check string "same string"
    "0000 OpAdd\n\
     0001 OpGetLocal 1\n\
     0003 OpConstant 2\n\
     0006 OpConstant 65534\n\
     0009 OpClosure 65535 255"
    (instructinos |> Code.to_string)

let () =
  let open Alcotest in
  run "Code"
    [
      ("make test", [test_case "make test" `Slow test_make]);
      ( "read operands test",
        [test_case "read operands test" `Slow test_read_operands] );
      ( "instructions to string test",
        [
          test_case "instructions to string test" `Slow
            test_instructions_to_string;
        ] );
    ]
