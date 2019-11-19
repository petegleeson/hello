type ast =
  | Call(Lexer.token, list(ast))
  | Number(Lexer.token)
  | Program(list(ast));

type program = list(ast);

let rec format = ast => {
  let join = (x, asts) => String.concat(x, List.map(format, asts));
  switch (ast) {
  | Number(t) => "Number(" ++ t.value ++ ")"
  | Program(asts) => "Program(\n" ++ join("\n", asts) ++ "\n)"
  | Call(fn, args) => "Call(" ++ fn.value ++ " [" ++ join(", ", args) ++ "])"
  };
};

exception ParseError(string);

let parse = (tokens: Stream.t(Lexer.token)) => {
  let rec match = () => {
    let lookahead = Stream.next(tokens);
    switch (lookahead.kind) {
    | Number => Number(lookahead)
    | LParen =>
      let next = Stream.next(tokens);
      let op =
        switch (next) {
        | {kind: Operator} => next
        | _ => raise(ParseError("unexpected token"))
        };
      let rec getArgs = () => {
        switch (Stream.peek(tokens)) {
        | Some({kind: RParen}) =>
          Stream.junk(tokens);
          [];
        | Some(node) =>
          let arg = match();
          [arg, ...getArgs()];
        | None => raise(ParseError("unexpected token"))
        };
      };
      Call(op, getArgs());
    | _ => raise(ParseError("unexpected token"))
    };
  };

  let rec getExps = () => {
    switch (Stream.peek(tokens)) {
    | Some(token) =>
      let exp = match();
      [exp, ...getExps()];
    | None => []
    };
  };

  Program(getExps());
};