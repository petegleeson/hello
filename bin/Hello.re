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

let nextToken = tokenize(filename);
let next = ref(nextToken());
while (next.contents.kind !== Eof) {
  print_endline(format(next.contents));
  next.contents = nextToken();
} /* print_endline(printAst(ast))*/ /* let ast = parse(nextToken)*/;