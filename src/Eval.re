open List;
open Parse;
open Lang;
open Utils;

open Belt.Result;

let rec eval(stack: stack, word: word): result(string, stack)=
  switch((word, stack)) {
  | (Push(lit), st) => Ok([lit, ...st]);
  | (Add, [Int(x), Int(y), ...st]) => Ok([Int(y + x), ...st]);
  | (Sub, [Int(x), Int(y), ...st]) => Ok([Int(y - x), ...st]);
  | (Mul, [Int(x), Int(y), ...st]) => Ok([Int(y * x), ...st]);
  | (Div, [Int(x), Int(y), ...st]) => Ok([Int(y / x), ...st]);
  | (Eq, [x, y, ...st]) => Ok([(x==y) ? True : False, ...st]);
  | (If, [Quotation(when_false), Quotation(when_true), b, ...st]) => to_bool(b) ? eval_words(st,when_true) : eval_words(st,when_false);
  | (Dup, [x, ...st]) => Ok([x, x, ...st]);
  | (Swap, [x ,y, ...st]) => Ok([y, x, ...st]);
  | (Rot, [x, y, z, ...st]) => Ok([z, x, y, ...st]);
  | (Drop, [_x, ...st]) => Ok(st);
  | (While, [Quotation(body), Quotation(pred), ...st]) => eval_while(st, body, pred);
  | _ => print_stack(stack); Error("improper stack contents");}
and eval_result(stack: result(string, stack), word: word): result(string, stack) =
    switch(stack) {
    | Error(e) => Error(e);
    | Ok(st)   => eval(st, word);
    }
and eval_words(stack: stack, words:list(word)): result(string, stack) = fold_left(eval_result, Ok(stack), words)
and eval_while_helper(stack: stack, body: list(word), pred: list(word)): result(string, stack) =
  switch(eval_words(stack, body)) {
  | Error(e) => Error(e);
  | Ok(st)   => eval_while(st, body, pred);
  }
and eval_while(stack: stack, body: list(word), pred: list(word)): result(string, stack) =
  switch(eval_words(stack, pred)) {
  | Ok([b,  ...st]) => to_bool(b) ? eval_while_helper(st, body, pred) : Ok(st);
  | Ok(_) => Error("eval_while empty stack");
  | Error(e) => Error(e);
  };

let interact(code: string): unit =
  switch(parse(code)) {
  | Error(e)  => print_endline(e);
  | Ok(words) => switch(eval_words([], words)){
                 | Error(e)  => print_endline(e);
                 | Ok(stack) => print_stack(stack);
                 };
  };

let run(stack: stack, code: string): result(string, stack) =
  switch(parse(code)) {
  | Error(f)  => Error(f);
  | Ok(words) => eval_words(stack, words);
  };

interact("1 dup 2 * +");
