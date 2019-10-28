open Lib.Lexer.Lexer;
open Lib.Lexer.Parser;

// let printTokens = token =>
//   switch (token) {
//   | TNumber(x) => Printf.printf("Number(%s)\n", x)
//   | TPlus => Printf.printf("Plus\n")
//   };

let rec printAst = ast => {
  "foo";
};

let filename = Sys.argv[1];
Stream.iter(
  token => print_endline(format(token)),
  filename |> open_in |> Stream.of_channel |> tokens,
);