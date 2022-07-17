open Monkey

let to_bytes arr = arr |> Array.map char_of_int |> Array.to_seq |> Bytes.of_seq

let test_make () =
  let open Alcotest in
  let open Code.OpCode in
  check bytes "same object"
    ([|OpConstant |> to_int; 255; 254|] |> to_bytes)
    (Code.make OpConstant [65534])

let _x : bytes = Bytes.of_string "abc"

let () =
  let open Alcotest in
  run "Parser" [("make test", [test_case "make test" `Slow test_make])]
