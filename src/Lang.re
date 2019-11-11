open List;

type literal
  = Int(int)
  | True
  | False
  | String(string)
  | List(list(literal))
  | Quotation(list(word))

and stack = list(literal)

and word
  = Push(literal)
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | If
  | Dup
  | Swap
  | Rot
  | Drop
  | While;

let print_literal(literal: literal): unit =
  switch (literal) {
  | Int(x)  => print_endline("Int(" ++ string_of_int(x) ++ ")");
  | True    => print_endline("True");
  | False    => print_endline("False");
  | String(_) => print_endline("String");
  | List(_) => print_endline("List");
  | Quotation(_) => print_endline("Quotation");
  }

let print_stack(stack: stack): unit =
  stack |> List.iter((elem: literal) => print_literal(elem));

let to_bool(literal: literal): bool =
  switch(literal) {
  | False => false;
  | _ => true;
  };

let rec eval(stack: stack, word: word): stack =
  switch((word, stack)) {
  | (Push(lit), st) => [lit, ...st];
  | (Add, [Int(x), Int(y), ...st]) => [Int(y + x), ...st];
  | (Sub, [Int(x), Int(y), ...st]) => [Int(y - x), ...st];
  | (Mul, [Int(x), Int(y), ...st]) => [Int(y * x), ...st];
  | (Div, [Int(x), Int(y), ...st]) => [Int(y / x), ...st];
  | (Eq, [x, y, ...st]) => [(x==y) ? True : False, ...st];
  | (If, [Quotation(when_false), Quotation(when_true), b, ...st]) => to_bool(b) ? eval_words(st,when_true) : eval_words(st,when_false);
  | (Dup, [x, ...st]) => [x, x, ...st];
  | (Swap, [x ,y, ...st]) => [y, x, ...st];
  | (Rot, [x, y, z, ...st]) => [z, x, y, ...st];
  | (Drop, [x, ...st]) => st;
  | (While, [Quotation(body), Quotation(pred), ...st]) => eval_while(st, body, pred);
  | _ => print_stack(stack); failwith("improper stack contents");
  }
and eval_words(stack: stack, words:list(word)): stack = fold_left(eval, stack, words)
and eval_while(stack: stack, body: list(word), pred: list(word)): stack =
  switch(eval_words(stack, pred)) {
  | [b,  ...st] => to_bool(b) ? eval_while(eval_words(st, body), body, pred) : st;
  | _ => failwith("eval_while empty stack");
  };
