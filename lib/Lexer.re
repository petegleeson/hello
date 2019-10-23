type filename = string;

type token =
  | TNumber(string)
  | TPlus;

type exp =
  | BinaryExp(exp, exp, exp)
  | Number(string)
  | Plus;

let isNumber = char => {
  let v = int_of_char(char);
  47 < v && v < 58;
};

let isPlus = char => {
  let v = int_of_char(char);
  v === 43;
};

let tokenize = filename => {
  let ic = open_in(filename);
  let next = () =>
    try(Some(input_char(ic))) {
    | End_of_file =>
      close_in(ic);
      None;
    };
  let rec match = char => {
    switch (char) {
    | None => []
    | Some(c) when isNumber(c) =>
      // buffer consecutive numbers
      switch (match(next())) {
      | [TNumber(n), ...rest] => [TNumber(Char.escaped(c) ++ n), ...rest]
      | x => [TNumber(Char.escaped(c)), ...x]
      }
    | Some(c) when isPlus(c) => [TPlus, ...match(next())]
    | Some(_) => match(next())
    };
  };
  match(next());
};

let rec parse = tokens => {
  exception ParseError;
  switch (tokens) {
  | [TNumber(x), TPlus, ...rest] => BinaryExp(Number(x), Plus, parse(rest))
  | [TNumber(n)] => Number(n)
  | _ => raise(ParseError)
  };
};