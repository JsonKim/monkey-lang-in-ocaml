open Monkey

let symbol_testable =
  Alcotest.testable Symbol_table.Symbol.pp Symbol_table.Symbol.equal

let test_define () =
  let open Alcotest in
  let open Symbol_table in
  check (list symbol_testable) "same object"
    [
      { Symbol.name = "a"; scope = Symbol_scope.GLOBAL; index = 0 };
      { Symbol.name = "b"; scope = Symbol_scope.GLOBAL; index = 1 };
      { Symbol.name = "c"; scope = Symbol_scope.LOCAL; index = 0 };
      { Symbol.name = "d"; scope = Symbol_scope.LOCAL; index = 1 };
      { Symbol.name = "e"; scope = Symbol_scope.LOCAL; index = 0 };
      { Symbol.name = "f"; scope = Symbol_scope.LOCAL; index = 1 };
    ]
    (let global = empty in
     let a, global = global |> define "a" in
     let b, global = global |> define "b" in

     let first_local = make_enclosed_symbol_table global in
     let c, first_local = first_local |> define "c" in
     let d, first_local = first_local |> define "d" in

     let second_local = make_enclosed_symbol_table first_local in
     let e, second_local = second_local |> define "e" in
     let f, _ = second_local |> define "f" in
     [a; b; c; d; e; f])

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

let test_resolve_local () =
  let open Alcotest in
  let open Symbol_table in
  check
    (list (option symbol_testable))
    "same object"
    [
      Some { Symbol.name = "a"; scope = Symbol_scope.GLOBAL; index = 0 };
      Some { Symbol.name = "b"; scope = Symbol_scope.GLOBAL; index = 1 };
      Some { Symbol.name = "c"; scope = Symbol_scope.LOCAL; index = 0 };
      Some { Symbol.name = "d"; scope = Symbol_scope.LOCAL; index = 1 };
    ]
    (let global = empty |> define "a" |> snd |> define "b" |> snd in
     let local =
       make_enclosed_symbol_table global
       |> define "c"
       |> snd
       |> define "d"
       |> snd in
     [
       local |> resolve "a";
       local |> resolve "b";
       local |> resolve "c";
       local |> resolve "d";
     ])

let test_resolve_nested_local () =
  let open Alcotest in
  let open Symbol_table in
  check
    (list (option symbol_testable))
    "same object"
    [
      Some { Symbol.name = "a"; scope = Symbol_scope.GLOBAL; index = 0 };
      Some { Symbol.name = "b"; scope = Symbol_scope.GLOBAL; index = 1 };
      Some { Symbol.name = "c"; scope = Symbol_scope.LOCAL; index = 0 };
      Some { Symbol.name = "d"; scope = Symbol_scope.LOCAL; index = 1 };
      Some { Symbol.name = "a"; scope = Symbol_scope.GLOBAL; index = 0 };
      Some { Symbol.name = "b"; scope = Symbol_scope.GLOBAL; index = 1 };
      Some { Symbol.name = "e"; scope = Symbol_scope.LOCAL; index = 0 };
      Some { Symbol.name = "f"; scope = Symbol_scope.LOCAL; index = 1 };
    ]
    (let global = empty |> define "a" |> snd |> define "b" |> snd in
     let first_local =
       make_enclosed_symbol_table global
       |> define "c"
       |> snd
       |> define "d"
       |> snd in
     let second_local =
       make_enclosed_symbol_table first_local
       |> define "e"
       |> snd
       |> define "f"
       |> snd in
     [
       first_local |> resolve "a";
       first_local |> resolve "b";
       first_local |> resolve "c";
       first_local |> resolve "d";
       second_local |> resolve "a";
       second_local |> resolve "b";
       second_local |> resolve "e";
       second_local |> resolve "f";
     ])

let test_define_resolve_builtins () =
  let open Alcotest in
  let open Symbol_table in
  check
    (list (option symbol_testable))
    "same object"
    [
      Some { Symbol.name = "a"; scope = Symbol_scope.BUILTIN; index = 0 };
      Some { Symbol.name = "c"; scope = Symbol_scope.BUILTIN; index = 1 };
      Some { Symbol.name = "e"; scope = Symbol_scope.BUILTIN; index = 2 };
      Some { Symbol.name = "f"; scope = Symbol_scope.BUILTIN; index = 3 };
      Some { Symbol.name = "a"; scope = Symbol_scope.BUILTIN; index = 0 };
      Some { Symbol.name = "c"; scope = Symbol_scope.BUILTIN; index = 1 };
      Some { Symbol.name = "e"; scope = Symbol_scope.BUILTIN; index = 2 };
      Some { Symbol.name = "f"; scope = Symbol_scope.BUILTIN; index = 3 };
      Some { Symbol.name = "a"; scope = Symbol_scope.BUILTIN; index = 0 };
      Some { Symbol.name = "c"; scope = Symbol_scope.BUILTIN; index = 1 };
      Some { Symbol.name = "e"; scope = Symbol_scope.BUILTIN; index = 2 };
      Some { Symbol.name = "f"; scope = Symbol_scope.BUILTIN; index = 3 };
    ]
    (let global =
       empty
       |> define_builtin 0 "a"
       |> snd
       |> define_builtin 1 "c"
       |> snd
       |> define_builtin 2 "e"
       |> snd
       |> define_builtin 3 "f"
       |> snd in
     let first_local = make_enclosed_symbol_table global in
     let second_local = make_enclosed_symbol_table first_local in
     [
       global |> resolve "a";
       global |> resolve "c";
       global |> resolve "e";
       global |> resolve "f";
       first_local |> resolve "a";
       first_local |> resolve "c";
       first_local |> resolve "e";
       first_local |> resolve "f";
       second_local |> resolve "a";
       second_local |> resolve "c";
       second_local |> resolve "e";
       second_local |> resolve "f";
     ])

let () =
  let open Alcotest in
  run "symbol_table"
    [
      ("define test", [test_case "define test" `Slow test_define]);
      ("resolve test", [test_case "resolve test" `Slow test_resolve]);
      ( "resolve local test",
        [test_case "resolve local test" `Slow test_resolve_local] );
      ( "resolve nested local test",
        [test_case "resolve nested local test" `Slow test_resolve_nested_local]
      );
      ( "define resolve builtin test",
        [
          test_case "define resolve builtin test" `Slow
            test_define_resolve_builtins;
        ] );
    ]
