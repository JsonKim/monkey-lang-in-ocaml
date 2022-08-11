module Symbol_scope = struct
  type t = GLOBAL [@@deriving show, eq]

  let compare a z = compare (a |> show) (z |> show)
end

module Symbol = struct
  type t = {
    name : string;
    scope : Symbol_scope.t;
    index : int;
  }
  [@@deriving show, eq]
end

module Store = struct
  module Internal = Map.Make (String)
  include Internal

  type t = Symbol.t Internal.t

  let pp fmt v =
    let box ?(indent = 0) pp_v ppf v =
      Format.(
        pp_open_box ppf indent;
        pp_v ppf v;
        pp_close_box ppf ()) in

    let iter fmt = Internal.iter (fun _ value -> Symbol.pp fmt value) in
    (box iter) fmt v

  let equal a b = a = b
end

type t = {
  store : Store.t;
  num_definitions : int;
}
[@@deriving show, eq]

let empty = { store = Store.empty; num_definitions = 0 }

let define name s =
  let store = s.store in
  let scope = Symbol_scope.GLOBAL in
  let index = s.num_definitions in
  let symbol = { Symbol.name; scope; index } in
  let store = store |> Store.add name symbol in

  let num_definitions = s.num_definitions + 1 in
  (symbol, { store; num_definitions })

let resolve name s = Store.find_opt name s.store
