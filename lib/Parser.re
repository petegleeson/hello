type ast =
  | Call(Lexer.token, list(ast))
  | Number(Lexer.token)
  | Program(list(ast));

type program = list(ast);

let rec format = ast => {
  let join = (x, asts) => String.concat(x, List.map(format, asts));
  switch (ast) {
  | Number(t) => "Number(" ++ t.value ++ ")"
  | Program(ast) => "Program(\n" ++ join("\n", ast) ++ "\n)"
  | Call(fn, args) => "Call(" ++ fn.value ++ " [" ++ join(", ", args) ++ "])"
  };
};

exception ParseError(string);

let parse = (tokens: Stream.t(Lexer.token)) => {
  let lookahead = prediction => {
    switch (Stream.peek(tokens)) {
    | Some(t) when t.kind === prediction =>
      Stream.junk(tokens);
      t;
    | _ => raise(ParseError("unexpected token"))
    };
  };

  let rec match = (token: Lexer.token) => {
    switch (token.kind) {
    | Number => Number(token)
    | LParen =>
      let op = lookahead(Operator);
      let args = [match(Stream.next(tokens)), match(Stream.next(tokens))];
      lookahead(RParen);
      Call(op, args);
    | _ => raise(ParseError("unexpected token"))
    };
  };

  Program([match(Stream.next(tokens))]);
};