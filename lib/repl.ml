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

let print env macro_env code =
  let go (p : Parser.t) env stmt =
    if List.length p.errors > 0 then (
      p.errors |> parse_list "\t" |> print_error;
      env)
    else
      let comp = Compiler.Compiler.empty in
      match Compiler.Compiler.compile comp stmt with
      | Error message ->
        print_endline message;
        env
      | Ok (comp, _) -> (
        let machine = comp |> Compiler.Compiler.to_bytecode |> Vm.make in
        let machine = machine |> Vm.run in
        match Vm.stack_top machine with
        | None ->
          print_endline "Woops! Executing bytecode failed";
          env
        | Some obj ->
          obj |> Object.show |> print_endline;
          env) in

  let l = Lexer.make code in
  let p = Parser.make l in
  let p, stmt = Parser.parse_program p in
  (go p env stmt, macro_env)

let run () =
  let rec go env macro_env =
    try
      print_string prompt;
      let line = read_line () in
      let env, macro_env = print env macro_env line in
      go env macro_env
    with
    | End_of_file -> () in
  let env = Environment.make () in
  let macro_env = Environment.make () in
  go env macro_env
