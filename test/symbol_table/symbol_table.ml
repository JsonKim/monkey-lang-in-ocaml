open Monkey

let store_testable =
  Alcotest.testable Symbol_table.Store.pp Symbol_table.Store.equal

let symbol_testable =
  Alcotest.testable Symbol_table.Symbol.pp Symbol_table.Symbol.equal

let test_define () =
  let open Alcotest in
  let open Symbol_table in
  check store_testable "same object"
    (Store.empty
    |> Store.add "a"
         { Symbol.name = "a"; scope = Symbol_scope.GLOBAL; index = 0 }
    |> Store.add "b"
         { Symbol.name = "b"; scope = Symbol_scope.GLOBAL; index = 1 })
    (let global = empty |> define "a" |> snd |> define "b" |> snd in
     global.store)

let test_resolve () =
  let open Alcotest in
  let open Symbol_table in
  check
    (list (option symbol_testable))
    "same object"
    [
      Some { Symbol.name = "a"; scope = Symbol_scope.GLOBAL; index = 0 };
      Some { Symbol.name = "b"; scope = Symbol_scope.GLOBAL; index = 1 };
    ]
    (let global = empty |> define "a" |> snd |> define "b" |> snd in
     [global |> resolve "a"; global |> resolve "b"])

let () =
  let open Alcotest in
  run "symbol_table"
    [
      ("define test", [test_case "define test" `Slow test_define]);
      ("resolve test", [test_case "resolve test" `Slow test_resolve]);
    ]
