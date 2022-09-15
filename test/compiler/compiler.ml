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
    {
      instructions = c |> Compiler.Compiler.current_instructions;
      constants = c.constants;
    }
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
  |> compile (make ())
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

let test_global_let_statement () =
  let open Alcotest in
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
                Code.make OpSetGlobal [0];
                Code.make OpConstant [1];
                Code.make OpSetGlobal [1];
              ];
          constants = [|Object.Integer 1; Object.Integer 2|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpSetGlobal [0];
                Code.make OpGetGlobal [0];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpSetGlobal [0];
                Code.make OpGetGlobal [0];
                Code.make OpSetGlobal [1];
                Code.make OpGetGlobal [1];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1|];
        };
    ]
    ([
       "let one = 1;\nlet two = 2;";
       "let one = 1;\none;";
       "let one = 1;\nlet two = one;\ntwo";
     ]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_string_expressions () =
  let open Alcotest in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions =
            concat_bytes [Code.make OpConstant [0]; Code.make OpPop []];
          constants = [|Object.String "monkey"|];
        };
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
          constants = [|Object.String "mon"; Object.String "key"|];
        };
    ]
    ([{|"monkey"|}; {|"mon" + "key"|}]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_array_literals () =
  let open Alcotest in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions =
            concat_bytes [Code.make OpArray [0]; Code.make OpPop []];
          constants = [||];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpConstant [2];
                Code.make OpArray [3];
                Code.make OpPop [];
              ];
          constants = [|Object.Integer 1; Object.Integer 2; Object.Integer 3|];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpAdd [];
                Code.make OpConstant [2];
                Code.make OpConstant [3];
                Code.make OpSub [];
                Code.make OpConstant [4];
                Code.make OpConstant [5];
                Code.make OpMul [];
                Code.make OpArray [3];
                Code.make OpPop [];
              ];
          constants =
            [|
              Object.Integer 1;
              Object.Integer 2;
              Object.Integer 3;
              Object.Integer 4;
              Object.Integer 5;
              Object.Integer 6;
            |];
        };
    ]
    (["[]"; "[1, 2, 3]"; "[1 + 2, 3 - 4, 5 * 6]"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_hash_literals () =
  let open Alcotest in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions = concat_bytes [Code.make OpHash [0]; Code.make OpPop []];
          constants = [||];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpConstant [2];
                Code.make OpConstant [3];
                Code.make OpConstant [4];
                Code.make OpConstant [5];
                Code.make OpHash [6];
                Code.make OpPop [];
              ];
          constants =
            [|
              Object.Integer 1;
              Object.Integer 2;
              Object.Integer 3;
              Object.Integer 4;
              Object.Integer 5;
              Object.Integer 6;
            |];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpConstant [2];
                Code.make OpAdd [];
                Code.make OpConstant [3];
                Code.make OpConstant [4];
                Code.make OpConstant [5];
                Code.make OpMul [];
                Code.make OpHash [4];
                Code.make OpPop [];
              ];
          constants =
            [|
              Object.Integer 1;
              Object.Integer 2;
              Object.Integer 3;
              Object.Integer 4;
              Object.Integer 5;
              Object.Integer 6;
            |];
        };
    ]
    (["{}"; "{1: 2, 3: 4, 5: 6}"; "{1: 2 + 3, 4: 5 * 6}"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_index_expressions () =
  let open Alcotest in
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
                Code.make OpConstant [2];
                Code.make OpArray [3];
                Code.make OpConstant [3];
                Code.make OpConstant [4];
                Code.make OpAdd [];
                Code.make OpIndex [];
                Code.make OpPop [];
              ];
          constants =
            [|
              Object.Integer 1;
              Object.Integer 2;
              Object.Integer 3;
              Object.Integer 1;
              Object.Integer 1;
            |];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                Code.make OpConstant [0];
                Code.make OpConstant [1];
                Code.make OpHash [2];
                Code.make OpConstant [2];
                Code.make OpConstant [3];
                Code.make OpSub [];
                Code.make OpIndex [];
                Code.make OpPop [];
              ];
          constants =
            [|
              Object.Integer 1;
              Object.Integer 2;
              Object.Integer 2;
              Object.Integer 1;
            |];
        };
    ]
    (["[1, 2, 3][1 + 1]"; "{1: 2}[2 - 1]"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_functions () =
  let open Alcotest in
  let open Code in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions = concat_bytes [make OpConstant [2]; make OpPop []];
          constants =
            [|
              Object.Integer 5;
              Object.Integer 10;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes
                      [
                        make OpConstant [0];
                        make OpConstant [1];
                        make OpAdd [];
                        make OpReturnValue [];
                      ];
                  num_locals = 0;
                };
            |];
        };
      Ok
        {
          instructions = concat_bytes [make OpConstant [2]; make OpPop []];
          constants =
            [|
              Object.Integer 5;
              Object.Integer 10;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes
                      [
                        make OpConstant [0];
                        make OpConstant [1];
                        make OpAdd [];
                        make OpReturnValue [];
                      ];
                  num_locals = 0;
                };
            |];
        };
      Ok
        {
          instructions = concat_bytes [make OpConstant [2]; make OpPop []];
          constants =
            [|
              Object.Integer 1;
              Object.Integer 2;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes
                      [
                        make OpConstant [0];
                        make OpPop [];
                        make OpConstant [1];
                        make OpReturnValue [];
                      ];
                  num_locals = 0;
                };
            |];
        };
    ]
    (["fn() { return 5 + 10 }"; "fn() { 5 + 10 }"; "fn() { 1; 2 }"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

module Scopes = struct
  let symbol_table_testable =
    Alcotest.testable Symbol_table.pp Symbol_table.equal

  let test_scope expected actual_c () =
    let open Alcotest in
    let open Compiler.Compiler in
    check
      (pair int (option symbol_table_testable))
      "same scope_index" expected
      (actual_c.scope_index, actual_c.symbol_table.outer)

  let run () =
    let open Compiler.Compiler in
    let open Alcotest in
    let compiler = make () in
    let global_symbol_table = Some compiler.symbol_table in
    let test_scope_index_before_enter = test_scope (0, None) compiler in

    let c, _ = emit compiler OpMul [] in
    let c = enter_scope c in
    let test_scope_after_enter = test_scope (1, global_symbol_table) c in

    let c, _ = emit c OpSub [] in
    let instructions = c.scopes.(c.scope_index).instructions in
    let test_scoped_instrunctions () =
      check string "only OpSub"
        (Code.make OpSub [] |> Code.to_string)
        (instructions |> Code.to_string) in

    let c = leave_scope c in
    let test_scope_index_after_leave = test_scope (0, None) c in

    let c, _ = emit c OpAdd [] in
    let instructions = c.scopes.(c.scope_index).instructions in
    let test_main_instrunctions () =
      check string "OpMul & OpAdd"
        ([Code.make OpMul []; Code.make OpAdd []]
        |> concat_bytes
        |> Code.to_string)
        (instructions |> Code.to_string) in

    [
      test_case "before enter" `Slow test_scope_index_before_enter;
      test_case "after enter" `Slow test_scope_after_enter;
      test_case "scoped instrunctions" `Slow test_scoped_instrunctions;
      test_case "after leave" `Slow test_scope_index_after_leave;
      test_case "main instrunctions" `Slow test_main_instrunctions;
    ]
end

let test_functions_without_return_value () =
  let open Alcotest in
  let open Code in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions = concat_bytes [make OpConstant [0]; make OpPop []];
          constants =
            [|
              Object.CompiledFunction
                {
                  instructions = concat_bytes [make OpReturn []];
                  num_locals = 0;
                };
            |];
        };
    ]
    (["fn() { }"]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_function_calls () =
  let open Alcotest in
  let open Code in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions =
            concat_bytes [make OpConstant [1]; make OpCall [0]; make OpPop []];
          constants =
            [|
              Object.Integer 24;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes [make OpConstant [0]; make OpReturnValue []];
                  num_locals = 0;
                };
            |];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                make OpConstant [1];
                make OpSetGlobal [0];
                make OpGetGlobal [0];
                make OpCall [0];
                make OpPop [];
              ];
          constants =
            [|
              Object.Integer 24;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes [make OpConstant [0]; make OpReturnValue []];
                  num_locals = 0;
                };
            |];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                make OpConstant [0];
                make OpSetGlobal [0];
                make OpGetGlobal [0];
                make OpConstant [1];
                make OpCall [1];
                make OpPop [];
              ];
          constants =
            [|
              Object.CompiledFunction
                {
                  instructions = concat_bytes [make OpReturn []];
                  num_locals = 0;
                };
              Object.Integer 24;
            |];
        };
      Ok
        {
          instructions =
            concat_bytes
              [
                make OpConstant [0];
                make OpSetGlobal [0];
                make OpGetGlobal [0];
                make OpConstant [1];
                make OpConstant [2];
                make OpConstant [3];
                make OpCall [3];
                make OpPop [];
              ];
          constants =
            [|
              Object.CompiledFunction
                {
                  instructions = concat_bytes [make OpReturn []];
                  num_locals = 0;
                };
              Object.Integer 24;
              Object.Integer 25;
              Object.Integer 26;
            |];
        };
    ]
    ([
       "fn() { 24 }();";
       "let noArg = fn() { 24 };\nnoArg();";
       "let oneArg = fn(a) { };\noneArg(24);\n";
       "let manyArg = fn(a, b, c) { };\nmanyArg(24, 25, 26);\n";
     ]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let test_let_statement_scopes () =
  let open Alcotest in
  let open Code in
  check
    (list (result compile_testable string))
    "same object"
    [
      Ok
        {
          instructions =
            concat_bytes
              [
                make OpConstant [0];
                make OpSetGlobal [0];
                make OpConstant [1];
                make OpPop [];
              ];
          constants =
            [|
              Object.Integer 55;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes [make OpGetGlobal [0]; make OpReturnValue []];
                  num_locals = 0;
                };
            |];
        };
      Ok
        {
          instructions = concat_bytes [make OpConstant [1]; make OpPop []];
          constants =
            [|
              Object.Integer 55;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes
                      [
                        make OpConstant [0];
                        make OpSetLocal [0];
                        make OpGetLocal [0];
                        make OpReturnValue [];
                      ];
                  num_locals = 1;
                };
            |];
        };
      Ok
        {
          instructions = concat_bytes [make OpConstant [2]; make OpPop []];
          constants =
            [|
              Object.Integer 55;
              Object.Integer 77;
              Object.CompiledFunction
                {
                  instructions =
                    concat_bytes
                      [
                        make OpConstant [0];
                        make OpSetLocal [0];
                        make OpConstant [1];
                        make OpSetLocal [1];
                        make OpGetLocal [0];
                        make OpGetLocal [1];
                        make OpAdd [];
                        make OpReturnValue [];
                      ];
                  num_locals = 2;
                };
            |];
        };
    ]
    ([
       "let num = 55;\nfn() { num }";
       "fn() {\n  let num = 55;\n  num\n}";
       "fn() {\n  let a = 55;\n  let b = 77;\n  a + b\n}";
     ]
    |> List.map (fun code -> code |> parser |> ast_to_test_compiler))

let () =
  let open Alcotest in
  run "Compiler"
    [
      ( "integer arithmetic test",
        [test_case "integer arithmetic test" `Slow test_integer_arithmetic] );
      ( "boolean expressions test",
        [test_case "boolean expression test" `Slow test_boolean_expressions] );
      ( "conditionals test",
        [test_case "conditionals test" `Slow test_conditionals] );
      ( "global let statement test",
        [test_case "global let statement test" `Slow test_global_let_statement]
      );
      ( "string expressions test",
        [test_case "string expressions test" `Slow test_string_expressions] );
      ( "array literals test",
        [test_case "array literals test" `Slow test_array_literals] );
      ( "hash literals test",
        [test_case "hash literals test" `Slow test_hash_literals] );
      ( "index expressions test",
        [test_case "index expressions test" `Slow test_index_expressions] );
      ("compiler scopes test", Scopes.run ());
      ("functions test", [test_case "functions test" `Slow test_functions]);
      ( "functions without return value test",
        [
          test_case "functions without return value test" `Slow
            test_functions_without_return_value;
        ] );
      ( "function calls test",
        [test_case "function calls test" `Slow test_function_calls] );
      ( "let statments scopes test",
        [test_case "let statement scopes test" `Slow test_let_statement_scopes]
      );
    ]
