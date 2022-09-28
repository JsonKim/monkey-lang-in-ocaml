open Monkey

let input =
  "let fibonacci = fn(x) {\n\
  \  if (x == 0) {\n\
  \    return 0;\n\
  \  } else {\n\
  \    if (x == 1) {\n\
  \      return 1;\n\
  \    } else {\n\
  \      fibonacci(x - 1) + fibonacci(x - 2);\n\
  \    }\n\
  \  }\n\
   };\n\
   fibonacci(35);"

let engine = "vm"

let main () =
  let open Bindings.Result in
  let l = Lexer.make input in
  let p = Parser.make l in
  let program = Parser.parse_program p in

  if engine = "vm" then
    let comp = Compiler.Compiler.make () in
    let+ comp, _ = Compiler.Compiler.compile comp (program |> snd) in
    let machine = Vm.make (Compiler.Compiler.to_bytecode comp) in
    let start_time = Sys.time () in
    let machine = Vm.run machine in
    let end_time = Sys.time () in
    Printf.printf "result=%s, durations=%fs\n"
      (machine |> Vm.last_popped_stack_elem |> Option.get |> Object.show)
      (end_time -. start_time)
  else
    let env = Environment.make () in
    let ast = program |> snd in
    let start_time = Sys.time () in
    let result = Evaluator.eval env ast in
    let end_time = Sys.time () in
    Printf.printf "result=%s, durations=%fs\n"
      (result |> fst |> Object.show)
      (end_time -. start_time);
    Ok ()

let () = main () |> ignore
