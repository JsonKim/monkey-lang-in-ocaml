let () =
  print_endline "Hello! This is the Monkey programming language!";
  print_endline "Feel free to type in commands";
  try
    while true do
      Monkey.Repl.run ()
    done
  with
  | End_of_file -> ()
