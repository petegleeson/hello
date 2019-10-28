type filename = string;

module Lexer = {
  type kind =
    | Number
    | LParen
    | RParen
    | Operator
    | Eof;

  type token = {
    kind,
    value: string,
  };

  let format = ({kind, value}) =>
    switch (kind) {
    | Number => "Number(" ++ value ++ ")"
    | LParen => "LParen"
    | RParen => "RParen"
    | Operator => "Operator(" ++ value ++ ")"
    | Eof => "Eof"
    };

  let isNumber = char => {
    let v = int_of_char(char);
    47 < v && v < 58;
  };

  let tokenize = filename => {
    let ic = open_in(filename);
    let next = () =>
      try(Some(input_char(ic))) {
      | End_of_file =>
        close_in(ic);
        None;
      };
    let rec nextToken = current => {
      switch (current) {
      | None => {kind: Eof, value: "EOF"}
      | Some(c) when c === '(' => {kind: LParen, value: "("}
      | Some(c) when c === ')' => {kind: RParen, value: ")"}
      | Some(c) when c === '+' || c === '-' || c === '/' || c === '*' => {
          kind: Operator,
          value: Char.escaped(c),
        }
      | Some(c) when isNumber(c) => {
          kind: Number,
          value: {
            switch (next()) {
            | Some(lookahead) when isNumber(lookahead) =>
              Char.escaped(c) ++ nextToken(Some(lookahead)).value
            | _ => Char.escaped(c)
            };
          },
        }
      | Some(_) => nextToken(next())
      };
    };
    () => nextToken(next());
  };
};

module Parser = {
  let parse = nextToken => {};
};

/*
 let rec parse = tokens => {
   exception ParseError;
   switch (tokens) {
   | [TNumber(x), TPlus, ...rest] => BinaryExp(Number(x), Plus, parse(rest))
   | [TNumber(n)] => Number(n)
   | _ => raise(ParseError)
   };
 };
 */