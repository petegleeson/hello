open Lib;

// let printTokens = token =>
//   switch (token) {
//   | TNumber(x) => Printf.printf("Number(%s)\n", x)
//   | TPlus => Printf.printf("Plus\n")
//   };

let filename = Sys.argv[1];
Stream.iter(
  token => print_endline(Lexer.format(token)),
  filename |> open_in |> Stream.of_channel |> Lexer.tokens,
);

print_endline("");

print_endline(
  Parser.format(
    Parser.parse(filename |> open_in |> Stream.of_channel |> Lexer.tokens),
  ),
);