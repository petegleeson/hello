type filename = string;

type token =
  | Number(char)
  | Plus;

let print = token =>
  switch (token) {
  | Number(x) => print_endline("Number")
  | Plus => print_endline("Plus")
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
    | Some(c) =>
      let v = int_of_char(c);
      if (47 < v && v < 58) {
        [Number(c), ...match(next())];
      } else if (v === 43) {
        [Plus, ...match(next())];
      } else {
        match(next());
      };
    | None => []
    };
  };
  let tokens = match(next());
  List.iter(print, tokens);
  ();
};