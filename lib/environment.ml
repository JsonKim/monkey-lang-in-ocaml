type 'a t = {
  env : (string * 'a) list;
  outer : 'a t option;
}
[@@deriving show, eq]

let make_enclosed outer = { env = []; outer = Some outer }
let make () = { env = []; outer = None }

let rec get key env =
  match List.assoc_opt key env.env with
  | Some value -> Some value
  | None ->
  match env.outer with
  | Some outer -> get key outer
  | None -> None

let set key value env =
  let outer = env.outer in
  let env = env.env |> List.remove_assoc key |> List.cons (key, value) in
  { env; outer }
