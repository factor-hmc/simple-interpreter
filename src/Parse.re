open Lang;
open List;
open Utils;
open Js.String;

let lex(str: string): list(string) =
  Js.String.split(str, "") |> Array.to_list;

// we could make 'string' a type parameter too, but this doesn't
// need to be extensible...
type parser('parsed) =
  list(string) => either(string, (list(string), 'parsed));

let run_parser(p: parser('a), input: list(string)): either(string, 'a) =
  switch(p(input)) {
  | Failure(f) => Failure(f);
  | Result(([], res)) => Result(res);
  | Result((toks, res)) => Failure("{j|Parser didn't consume all input.|j}");
  }

let bind(p: parser('a), next: 'a => parser('b)): parser('b) =
  (toks: list(string)) => {
    switch(p(toks)) {
    | Failure(f) => Failure(f);
    | Result((new_toks, res)) => next(res)(new_toks);
    };
  };

let try_both(p1: parser('a), p2: parser('a)) =
  (toks: list(string)) => {
    switch(p1(toks)) {
    | Failure(_f) => p2(toks);
    | Result((new_toks, res)) => Result((new_toks, res));
    }
  };

let fail(message: string) = (_toks: list(string)) => Failure(message);

let pure(res: 'parsed): parser('parsed) =
  (toks: list(string)) => Result((toks, res));

let (>>=) = bind;
let (<|>) = try_both

let rec many(p: parser('a)): parser(list('a)) =
  some(p) <|> pure([])
and some(p: parser('a)): parser(list('a)) =
  p >>= ((res) => many(p) >>=
         ((reses) => pure([res, ...reses])));

let between(start_p: parser('start_p), end_p: parser('end_p), p: parser('a)): parser('a) =
  start_p >>=
  ( (_) => p >>=
           ( (a) => end_p >>=
              ( (_) => pure(a)
           )
  )
  );

let choice(ps: list(parser('a))): parser('a) =
  fold_left(try_both, fail("choice: No parsers given"),ps);

let builtins: list((string, word)) =
  [ ("+", Add)
  , ("-", Sub)
  , ("*", Mul)
  , ("/", Div)
  , ("=", Eq)
  , ("if", If)
  , ("dup", Dup)
  , ("swap", Swap)
  , ("rot", Rot)
  , ("drop", Drop)
  , ("while", While)
  ];

// let builtin_p: parser(word);
let builtin_p(toks: list(string)) =
  switch(toks) {
  | [] => Failure("builtin: empty input");
  | [t, ...ts] => switch(lookup(t, builtins)){
                  | None    => Failure({j|builtin: Not a builtin: $t|j});
                  | Some(b) => Result((ts, b));
                  };
  };

let symbol_p(symb: string): parser(string) =
  (toks: list(string)) => {
    switch(toks) {
    | [t, ...ts] when t == symb => Result((ts, symb));
    | _ => Failure({j|Failed to parse symbol $symb|j});
    }
  };

// let integer_p: parser(literal);
let integer_p(toks: list(string)) =
  switch(toks) {
  | [] => Failure("integer: empty input");
  | [t, ...ts] => switch(int_of_string(t)) {
                  | exception _ => Failure({j|integer: cannot convert $t to integer|j});
                  | n    => Result((ts, Int(n)));
                  };
  };

// let boolean_p: parser(literal);
let boolean_p(toks: list(string)) =
  switch(toks) {
  | [] => Failure("boolean: empty input");
  | [t, ...ts] => switch(t) {
                  | "t" => Result((ts, True));
                  | "f" => Result((ts, False));
                  | _   => Failure({j|boolean: cannot convert $t to boolean|j});
                  };
  };

let rec words_p_((): unit): parser(list(word)) = many(word_p_(()))
and word_p_((): unit): parser(word) =
  choice([builtin_p , push_p_(())])
and push_p_((): unit): parser(word) =
  literal_p_(()) >>= (lit) => pure(Push(lit))
and literal_p_((): unit): parser(literal) =
  choice([integer_p , boolean_p, list_p_(()), quotation_p_(())])
and list_p_((): unit): parser(literal) =
  between(symbol_p("{"), symbol_p("}"), literal_p_ ())
and quotation_p_((): unit): parser(literal) =
  between(symbol_p("["), symbol_p("]"),
          (words_p_(()) >>= (words) => pure(Quotation(words)))
  );

let words_p = words_p_(());

let parse(code: string): either(string, list(word)) =
  lex(code) |> run_parser(words_p);
