open Parser;

type result =
  | RVoid
  | RNumber(int)
  | RList(list(result));

exception RuntimeError(string);

let rec interpret = ast => {
  switch (ast) {
  | Program(exps) => RList(List.map(interpret, exps))
  | Call(op, args) =>
    let args = List.map(interpret, args);
    switch (op) {
    | {kind: Operator} =>
      let fn =
        switch (op) {
        | {kind: Operator, value: "+"} => ((a, b) => a + b)
        | {kind: Operator, value: "-"} => ((a, b) => a - b)
        | {kind: Operator, value: "*"} => ((a, b) => a * b)
        | {kind: Operator, value: "/"} => ((a, b) => a / b)
        | _ => raise(RuntimeError("invalid operator"))
        };
      switch (args) {
      | [initial, ...rest] =>
        List.fold_left(
          (a, b) =>
            switch (a, b) {
            | (RNumber(x), RNumber(y)) => RNumber(fn(x, y))
            | _ =>
              raise(RuntimeError("operators can only be applied to numbers"))
            },
          initial,
          rest,
        )
      | _ => raise(RuntimeError("invalid amount of args"))
      };
    | {kind: Id, value: "print"} =>
      List.iter(
        x =>
          switch (x) {
          | RNumber(n) =>
            print_int(n);
            print_newline();
          | _ => raise(RuntimeError("cant print this"))
          },
        args,
      );
      RVoid;
    | _ => raise(RuntimeError("unknown operator"))
    };
  | Number({value}) => RNumber(int_of_string(value))
  };
};