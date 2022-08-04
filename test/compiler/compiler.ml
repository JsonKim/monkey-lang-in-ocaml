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

let compile_testable =
  let open Compiler.Compiler in
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

let test_integer_arithmetic () =
  let open Alcotest in
  let open Code.OpCode in
  let open Compiler.Compiler in
  check
    (result compile_testable string)
    "same object"
    (Ok
       {
         Compiler.Compiler.instructions =
           concat_bytes
             [
               Code.make OpConstant [0];
               Code.make OpConstant [1];
               Code.make OpAdd [];
               Code.make OpPop [];
             ];
         constants = [|Object.Integer 1; Object.Integer 2|];
       })
    ("1 + 2" |> parser |> compile empty |> Result.map fst)

let () =
  let open Alcotest in
  run "Parser"
    [("make test", [test_case "make test" `Slow test_integer_arithmetic])]
