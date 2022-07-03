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

let print code =
  let go (p : Parser.t) stmt =
    if List.length p.errors > 0 then
      p.errors |> parse_list "\t" |> print_error
    else
      stmt |> Evaluator.eval_program |> Object.show |> print_endline in

  let l = Lexer.make code in
  let p = Parser.make l in
  let p, stmt = Parser.parse_program p in
  go p stmt

let run () =
  print_string prompt;
  let line = read_line () in
  print line
