module Symbol_scope = struct
  type t =
    | GLOBAL
    | LOCAL
    | BUILTIN
    | FREE
  [@@deriving show, eq]

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
  outer : t option;
  store : Store.t;
  num_definitions : int;
  free_symbols : Symbol.t array;
}
[@@deriving show, eq]

let empty =
  {
    outer = None;
    store = Store.empty;
    num_definitions = 0;
    free_symbols = [||];
  }

let make_enclosed_symbol_table outer = { empty with outer = Some outer }

let define name s =
  let store = s.store in
  let scope =
    match s.outer with
    | None -> Symbol_scope.GLOBAL
    | Some _ -> Symbol_scope.LOCAL in
  let index = s.num_definitions in
  let symbol = { Symbol.name; scope; index } in
  let store = store |> Store.add name symbol in

  let num_definitions = s.num_definitions + 1 in
  (symbol, { s with store; num_definitions })

let define_free original s =
  let free_symbols = Array.append s.free_symbols [|original|] in

  let symbol =
    { original with scope = FREE; index = Array.length free_symbols - 1 } in
  let store = Store.update original.name (fun _ -> Some symbol) s.store in

  (Some symbol, { s with store; free_symbols })

let define_builtin index name s =
  let symbol = { Symbol.name; index; scope = BUILTIN } in
  let store = s.store |> Store.add name symbol in
  (symbol, { s with store })

let resolve_local name s = Store.find_opt name s.store

let rec resolve_outer name s =
  match Option.bind s.outer (resolve_local name) with
  | Some symbol -> Some symbol
  | None -> Option.bind s.outer (resolve_outer name)

let resolve name s =
  match resolve_local name s with
  | Some symbol -> Some symbol
  | None -> resolve_outer name s

let rec resolve_free name s =
  let open Symbol in
  let obj = resolve_local name s in
  match (obj, s.outer) with
  | None, Some outer -> (
    let obj, outer = resolve_free name outer in
    let s = { s with outer = Some outer } in
    match obj with
    | None -> (obj, s)
    | Some { scope = GLOBAL; _ }
    | Some { scope = BUILTIN; _ } ->
      (obj, s)
    | Some obj ->
      let free, s = define_free obj s in
      (free, s))
  | _ -> (obj, s)
