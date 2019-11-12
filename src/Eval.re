open List;
open Parse;
open Lang;

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
  | (Drop, [_x, ...st]) => st;
  | (While, [Quotation(body), Quotation(pred), ...st]) => eval_while(st, body, pred);
  | _ => print_stack(stack); failwith("improper stack contents");
  }
and eval_words(stack: stack, words:list(word)): stack = fold_left(eval, stack, words)
and eval_while(stack: stack, body: list(word), pred: list(word)): stack =
  switch(eval_words(stack, pred)) {
  | [b,  ...st] => to_bool(b) ? eval_while(eval_words(st, body), body, pred) : st;
  | _ => failwith("eval_while empty stack");
  };

let interact(code: string): unit =
  switch(parse(code)) {
  | Failure(f) => print_endline(f);
  | Result(words) => print_stack(eval_words([], words));
  }

interact("1 2 +");
