open Monkey

module Fmt = struct
  let surround s1 s2 pp_v ppf v =
    Format.(
      pp_print_string ppf s1;
      pp_v ppf v;
      pp_print_string ppf s2)

  let box ?(indent = 0) pp_v ppf v =
    Format.(
      pp_open_box ppf indent;
      pp_v ppf v;
      pp_close_box ppf ())

  let oxford_brackets pp_v = box ~indent:2 (surround "[|" "|]" pp_v)

  let semi ppf _ =
    let sp ppf _ = Format.pp_print_space ppf () in
    Format.pp_print_string ppf ";";
    sp ppf ()

  let iter ~sep:pp_sep iter pp_elt ppf v =
    let is_first = ref true in
    let pp_elt v =
      if !is_first then is_first := false else pp_sep ppf ();
      pp_elt ppf v in
    iter pp_elt v

  let array ~sep pp_elt = iter ~sep Array.iter pp_elt
  let array pp_elt = oxford_brackets (array ~sep:semi (box pp_elt))
end

module Compiler_Test = struct
  type t = {
    instructions : Code.instructions;
    constants : Object.t array;
  }
  [@@deriving eq]

  let convert c =
    { instructions = c.Compiler.Compiler.instructions; constants = c.constants }
end

let compile_testable =
  let open Compiler_Test in
  let pp_compiler fmt compiler =
    Format.pp_print_string fmt "\n";
    (Fmt.array Object.pp) fmt compiler.constants;
    Format.pp_print_string fmt "\n";
    Format.fprintf fmt "%s" (Code.to_string compiler.instructions) in

  Alcotest.testable pp_compiler equal

let concat_bytes l =
  List.fold_right (fun b acc -> Bytes.cat b acc) l Bytes.empty

let parser code =
  code |> Lexer.make |> Parser.make |> Parser.parse_program |> snd

let ast_to_test_compiler ast =
  let open Compiler.Compiler in
  ast
  |> compile empty
  |> Result.map (fun x -> x |> fst |> Compiler_Test.convert)

let test_integer_arithmetic () =
  let open Alcotest in
  let open Code.OpCode in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpAdd [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpPop [];
                Code.make OpConstant [1];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpSub [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpMul [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpDiv [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 2; Object.Integer 1|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpMinus [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1|];
        };
    ]
    (["1 + 2"; "1; 2"; "1 - 2"; "1 * 2"; "2 / 1"; "-1"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_boolean_expressions () =
  let open Alcotest in
  let open Code.OpCode in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions = concat_bytes [Code.make OpTrue []; Code.make OpPop []];
          constants = [||];
        };
      Ok
        {
          instructions = concat_bytes [Code.make OpFalse []; Code.make OpPop []];
          constants = [||];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpGreaterThan [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpGreaterThan [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 2; Object.Integer 1|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpEqual [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpNotEqual [];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpTrue [];
                Code.make OpFalse [];
                Code.make OpEqual [];
                Code.make OpPop [];
              ];
          constants = [||];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpTrue [];
                Code.make OpFalse [];
                Code.make OpNotEqual [];
                Code.make OpPop [];
              ];
          constants = [||];
        };
      Ok
        {
          instructions =
            concat_bytes
              [Code.make OpTrue []; Code.make OpBang []; Code.make OpPop []];
          constants = [||];
        };
    ]
    ([
       "true";
       "false";
       "1 > 2";
       "1 < 2";
       "1 == 2";
       "1 != 2";
       "true == false";
       "true != false";
       "!true";
     ]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_conditionals () =
  let open Alcotest in
  let open Code.OpCode in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpTrue [];
                Code.make OpJumpNotTruthy [10];
                Code.make OpConstant [0];
                Code.make OpJump [11];
                Code.make OpNull [];
                Code.make OpPop [];
                Code.make OpConstant [1];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 10; Object.Integer 3333|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpTrue [];
                Code.make OpJumpNotTruthy [10];
                Code.make OpConstant [0];
                Code.make OpJump [13];
                Code.make OpConstant [1];
                Code.make OpPop [];
                Code.make OpConstant [2];
                Code.make OpPop [];
              ];
          constants =
            [|Object.Integer 10; Object.Integer 20; Object.Integer 3333|];
        };
    ]
    (["if (true) { 10 }; 3333"; "if (true) { 10 } else { 20 }; 3333;"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "integer arithmetic test",
        [test_case "integer arithmetic test" `Slow test_integer_arithmetic] );
      ( "boolean expressions test",
        [test_case "boolean expression test" `Slow test_boolean_expressions] );
      ( "conditionals test",
        [test_case "conditionals test" `Slow test_conditionals] );
    ]
