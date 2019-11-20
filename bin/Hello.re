open Lib;

switch (Sys.argv) {
| [|_, filename|] =>
  filename
  |> open_in
  |> Stream.of_channel
  |> Lexer.tokens
  |> Parser.parse
  |> Interpreter.interpret
| _ =>
  print_endline("need to provide a source file");
  Interpreter.RVoid;
};