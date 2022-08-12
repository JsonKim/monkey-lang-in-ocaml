let prompt = ">> "

let monkey =
  {|            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----''|}

let parse_list padding list =
  List.fold_right (fun x y -> padding ^ x ^ "\n" ^ y) list ""

let print_error errors =
  print_endline monkey;
  print_endline "Woops! We ran into some monkey business here!";
  print_endline errors

let print env macro_env code constants symbol_table globals =
  let go (p : Parser.t) env stmt =
    if List.length p.errors > 0 then (
      p.errors |> parse_list "\t" |> print_error;
      (env, constants, symbol_table))
    else
      let comp = Compiler.Compiler.make_with_state symbol_table constants in
      match Compiler.Compiler.compile comp stmt with
      | Error message ->
        print_endline message;
        (env, comp.constants, comp.symbol_table)
      | Ok (comp, _) -> (
        let machine =
          comp
          |> Compiler.Compiler.to_bytecode
          |> Vm.make_with_globals_store globals in
        let machine = machine |> Vm.run in
        match Vm.last_popped_stack_elem machine with
        | None ->
          print_endline "Woops! Executing bytecode failed";
          (env, comp.constants, comp.symbol_table)
        | Some obj ->
          obj |> Object.show |> print_endline;
          (env, comp.constants, comp.symbol_table)) in

  let l = Lexer.make code in
  let p = Parser.make l in
  let p, stmt = Parser.parse_program p in
  let env, constants, symbol_table = go p env stmt in
  (env, macro_env, constants, symbol_table)

let run () =
  let globals = Vm.empty_globals in
  let rec go env macro_env constants symbol_table =
    try
      print_string prompt;
      let line = read_line () in
      let env, macro_env, constants, symbol_table =
        print env macro_env line constants symbol_table globals in
      go env macro_env constants symbol_table
    with
    | End_of_file -> () in
  let env = Environment.make () in
  let macro_env = Environment.make () in
  let constants = [||] in
  let symbol_table = Symbol_table.empty in
  go env macro_env constants symbol_table
