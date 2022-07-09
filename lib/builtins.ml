module Builtin = Map.Make (String)

let len args =
  match args with
  | [Object.String str] -> Object.Integer (str |> String.length)
  | [Object.Array arr] -> Object.Integer (arr |> List.length)
  | [arg] ->
    Object.Error
      ("argument to len not supported, got " ^ Object.decode_tag_of arg)
  | _ ->
    Object.Error
      ("wrong number of arguments. got="
      ^ (args |> List.length |> string_of_int)
      ^ ", want=1")

let first args =
  match args with
  | [Object.Array []] -> Object.Null
  | [Object.Array (ele :: _)] -> ele
  | [arg] ->
    Object.Error
      ("argument to first must be Array, got " ^ Object.decode_tag_of arg)
  | _ ->
    Object.Error
      ("wrong number of arguments. got="
      ^ (args |> List.length |> string_of_int)
      ^ ", want=1")

let last args =
  match args with
  | [Object.Array []] -> Object.Null
  | [Object.Array arr] -> arr |> List.rev |> List.hd
  | [arg] ->
    Object.Error
      ("argument to last must be Array, got " ^ Object.decode_tag_of arg)
  | _ ->
    Object.Error
      ("wrong number of arguments. got="
      ^ (args |> List.length |> string_of_int)
      ^ ", want=1")

let rest args =
  match args with
  | [Object.Array arr] -> Object.Array (arr |> List.tl)
  | [arg] ->
    Object.Error
      ("argument to rest must be Array, got " ^ Object.decode_tag_of arg)
  | _ ->
    Object.Error
      ("wrong number of arguments. got="
      ^ (args |> List.length |> string_of_int)
      ^ ", want=1")

let push args =
  match args with
  | [Object.Array arr; ele] -> Object.Array (arr @ [ele])
  | [arg1; _] ->
    Object.Error
      ("argument to rest must be Array, got " ^ Object.decode_tag_of arg1)
  | _ ->
    Object.Error
      ("wrong number of arguments. got="
      ^ (args |> List.length |> string_of_int)
      ^ ", want=1")

let fns =
  Builtin.empty
  |> Builtin.add "len" (Object.Builtin { fn = len })
  |> Builtin.add "first" (Object.Builtin { fn = first })
  |> Builtin.add "last" (Object.Builtin { fn = last })
  |> Builtin.add "rest" (Object.Builtin { fn = rest })
  |> Builtin.add "push" (Object.Builtin { fn = push })
