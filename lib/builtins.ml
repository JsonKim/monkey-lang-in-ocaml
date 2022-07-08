module Builtin = Map.Make (String)

let len args =
  match args with
  | [Object.String str] -> Object.Integer (str |> String.length)
  | [arg] ->
    Object.Error
      ("argument to len not supported, got " ^ Object.decode_tag_of arg)
  | _ ->
    Object.Error
      ("wrong number of arguments. got="
      ^ (args |> List.length |> string_of_int)
      ^ ", want=1")

let fns = Builtin.empty |> Builtin.add "len" (Object.Builtin { fn = len })
