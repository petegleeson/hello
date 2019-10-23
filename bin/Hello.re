open Lib.Lexer;

let printTokens = token =>
  switch (token) {
  | TNumber(x) => Printf.printf("Number(%s)\n", x)
  | TPlus => Printf.printf("Plus\n")
  };

let rec printExp = exp => {
  switch (exp) {
  | BinaryExp(x, o, y) =>
    Printf.sprintf(
      "BinaryExp(%s, %s, %s)\n",
      printExp(x),
      printExp(o),
      printExp(y),
    )
  | Number(x) => Printf.sprintf("Number(%s)\n", x)
  | Plus => "Plus\n"
  };
};

let tokens = tokenize(Sys.argv[1]);
List.iter(printTokens, tokens);

let exp = parse(tokens);
print_endline(printExp(exp));