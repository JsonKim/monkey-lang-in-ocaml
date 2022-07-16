open Monkey

module To_test = struct
  let parse lex = lex |> Parser.make |> Parser.parse_program |> snd
end

let object_testable = Alcotest.testable Object.pp_option_t Object.equal_option_t
let ast_node_testable = Alcotest.testable Ast.pp_node Ast.equal_node

let test_define_macros () =
  let code =
    "let number = 1\n\
     let function = fn(x, y) { x + y; }\n\
     let mymacro = macro(x, y) { x + y; }" in
  let macros =
    code
    |> Lexer.make
    |> To_test.parse
    |> Macro_expansion.define_macros (Environment.make ()) in
  let env = macros |> fst in
  print_endline (macros |> snd |> Ast.show_node);
  Alcotest.(check (list object_testable))
    "same object"
    [
      Environment.get "number" env;
      Environment.get "function" env;
      Environment.get "mymacro" env;
    ]
    [
      None;
      None;
      Some
        (Object.Macro
           {
             parameters = ["x"; "y"];
             body =
               [
                 Ast.ExpressionStatement
                   {
                     expression =
                       Ast.Infix
                         {
                           token = Token.Plus;
                           left = Ast.Identifier "x";
                           right = Ast.Identifier "y";
                         };
                   };
               ];
             env = { Environment.env = []; outer = None };
           });
    ]

let test_expand_macros () =
  let code =
    [
      "let infix_expression = macro() { quote(1 + 2); };\n\ninfix_expression();";
      "let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };\n\n\
       reverse(2 + 2, 10 - 5);";
      {|
let unless = macro(condition, consequence, alternative) {
  quote(if (!(unquote(condition))) {
    unquote(consequence);
  } else {
    unquote(alternative);
  })
};

unless(10 < 5, puts("not greater"), puts("greater"));
|};
    ] in
  let nodes =
    code
    |> List.map (fun code ->
           let env, node =
             code
             |> Lexer.make
             |> To_test.parse
             |> Macro_expansion.define_macros (Environment.make ()) in
           Macro_expansion.expand_macros env node) in
  Alcotest.(check (list ast_node_testable))
    "same object"
    [
      Ast.Program
        [
          Ast.ExpressionStatement
            {
              expression =
                Ast.Infix
                  {
                    token = Token.Plus;
                    left = Ast.Literal (Ast.Integer 1);
                    right = Ast.Literal (Ast.Integer 2);
                  };
            };
        ];
      Ast.Program
        [
          Ast.ExpressionStatement
            {
              expression =
                Ast.Infix
                  {
                    token = Token.Minus;
                    left =
                      Ast.Infix
                        {
                          token = Token.Minus;
                          left = Ast.Literal (Ast.Integer 10);
                          right = Ast.Literal (Ast.Integer 5);
                        };
                    right =
                      Ast.Infix
                        {
                          token = Token.Plus;
                          left = Ast.Literal (Ast.Integer 2);
                          right = Ast.Literal (Ast.Integer 2);
                        };
                  };
            };
        ];
      Ast.Program
        [
          Ast.ExpressionStatement
            {
              expression =
                Ast.If
                  {
                    condition =
                      Ast.Prefix
                        {
                          token = Token.Bang;
                          right =
                            Ast.Infix
                              {
                                token = Token.LT;
                                left = Ast.Literal (Ast.Integer 10);
                                right = Ast.Literal (Ast.Integer 5);
                              };
                        };
                    consequence =
                      [
                        Ast.ExpressionStatement
                          {
                            expression =
                              Ast.Call
                                {
                                  fn = Ast.Identifier "puts";
                                  arguments =
                                    [Ast.Literal (Ast.String "not greater")];
                                };
                          };
                      ];
                    alternative =
                      Some
                        [
                          Ast.ExpressionStatement
                            {
                              expression =
                                Ast.Call
                                  {
                                    fn = Ast.Identifier "puts";
                                    arguments =
                                      [Ast.Literal (Ast.String "greater")];
                                  };
                            };
                        ];
                  };
            };
        ];
    ]
    nodes

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "define macros test",
        [test_case "define macros" `Slow test_define_macros] );
      ( "expand macros test",
        [test_case "expand macros" `Slow test_expand_macros] );
    ]
