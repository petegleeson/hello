type filename = string;

type token =
  | Number(string)
  | Plus;

let print = token =>
  switch (token) {
  | Number(x) => Printf.printf("Number(%s)\n", x)
  | Plus => Printf.printf("Plus\n")
  };

let isNumber = char => {
  let v = int_of_char(char);
  47 < v && v < 58;
};

let isPlus = char => {
  let v = int_of_char(char);
  v === 43;
};

let parse = filename => {
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
      | [Number(n), ...rest] => [Number(Char.escaped(c) ++ n), ...rest]
      | x => [Number(Char.escaped(c)), ...x]
      }
    | Some(c) when isPlus(c) => [Plus, ...match(next())]
    | Some(_) => match(next())
    };
  };
  let tokens = match(next());
  List.iter(print, tokens);
  ();
};