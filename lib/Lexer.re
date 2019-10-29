type filename = string;

type kind =
  | Number
  | LParen
  | RParen
  | Operator;

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
  };

let isNumber = char => {
  let v = int_of_char(char);
  47 < v && v < 58;
};

let tokens = chars => {
  let rec tokenize = i =>
    try(
      switch (Stream.next(chars)) {
      | c when c === '(' => Some({kind: LParen, value: "("})
      | c when c === ')' => Some({kind: RParen, value: ")"})
      | c when c === '+' || c === '-' || c === '/' || c === '*' =>
        Some({kind: Operator, value: Char.escaped(c)})
      | c when isNumber(c) =>
        Some({
          kind: Number,
          value:
            switch (Stream.peek(chars)) {
            | Some(lookahead) when isNumber(lookahead) =>
              Char.escaped(c)
              ++ (
                switch (tokenize(i)) {
                | Some({kind: Number, value}) => value
                | _ => "" // this is bad because we should always match above
                }
              )
            | _ => Char.escaped(c)
            },
        })
      | _ => tokenize(i)
      }
    ) {
    | Stream.Failure => None
    };
  Stream.from(tokenize);
};