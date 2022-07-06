module Env = Map.Make (String)

let make () = Env.empty
let get key env = Env.find_opt key env
let set key value env = Env.add key value env
