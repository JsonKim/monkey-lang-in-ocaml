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
      let obj, env = stmt |> Evaluator.eval env in
      obj |> Object.show |> print_endline;
      env in

  let l = Lexer.make code in
  let p = Parser.make l in
  let p, stmt = Parser.parse_program p in
  let macro_env, define_macros = Macro_expansion.define_macros macro_env stmt in
  let expanded = Macro_expansion.expand_macros macro_env define_macros in
  (go p env expanded, macro_env)

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
