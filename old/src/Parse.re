open Lang;
open List;
open Utils;

open Belt.Result;
open Js.String;

// lex only on spaces for now
let lex = (str: string): list(string) =>
  Js.String.split(" ", str) |> Array.to_list |> filter(s => length(s) > 0);

// we could make 'string' a type parameter too, but this doesn't
// need to be extensible...
type parser('parsed) =
  list(string) => result(string, (list(string), 'parsed));

type parsed('parsed) = result(string, (list(string), 'parsed));

let run_parser = (p: parser('a), input: list(string)): result(string, 'a) =>
  switch (p(input)) {
  | Error(f) => Error(f)
  | Ok(([], res)) => Ok(res)
  | Ok((toks, _res)) =>
    Error({j|Parser didn't consume all input, [$toks] remaining.|j})
  };

let bind = (p: parser('a), next: 'a => parser('b)): parser('b) =>
  (toks: list(string)) => {
    switch (p(toks)) {
    | Error(f) => Error(f)
    | Ok((new_toks, res)) => next(res, new_toks)
    };
  };
let (>>=) = bind;

let try_both = (p1: parser('a), p2: parser('a), toks: list(string)) => {
  switch (p1(toks)) {
  | Error(_f) => p2(toks)
  | Ok((new_toks, res)) => Ok((new_toks, res))
  };
};
let (<|>) = try_both;

let fail = (message: string, _toks: list(string)) => Error(message);

let pure = (res: 'parsed): parser('parsed) =>
  (toks: list(string)) => Ok((toks, res));

let rec many = (p: parser('a)): parser(list('a)) => some(p) <|> pure([])
and some = (p: parser('a)): parser(list('a)) =>
  p >>= (res => many(p) >>= (reses => pure([res, ...reses])));

let between =
    (start_p: parser('start_p), end_p: parser('end_p), p: parser('a))
    : parser('a) =>
  start_p >>= (_ => p >>= (a => end_p >>= (_ => pure(a))));

let choice = (ps: list(parser('a))): parser('a) =>
  fold_left(try_both, fail("choice: No parsers given"), ps);

let builtins: list((string, word)) = [
  ("+", Add),
  ("-", Sub),
  ("*", Mul),
  ("/", Div),
  ("=", Eq),
  ("if", If),
  ("dup", Dup),
  ("swap", Swap),
  ("rot", Rot),
  ("drop", Drop),
  ("while", While),
  ("clear", Clear),
];

// let builtin_p: parser(word);
let builtin_p = (toks: list(string)) =>
  switch (toks) {
  | [] => Error("builtin: empty input")
  | [t, ...ts] =>
    switch (lookup(t, builtins)) {
    | None => Error({j|builtin: Not a builtin: $t|j})
    | Some(b) => Ok((ts, b))
    }
  };

let symbol_p = (symb: string): parser(string) =>
  (toks: list(string)) => {
    switch (toks) {
    | [t, ...ts] when t == symb => Ok((ts, symb))
    | _ => Error({j|Failed to parse symbol $symb|j})
    };
  };

// let integer_p: parser(literal);
let integer_p = (toks: list(string)) =>
  switch (toks) {
  | [] => Error("integer: empty input")
  | [t, ...ts] =>
    switch (int_of_string(t)) {
    | exception _ => Error({j|integer: cannot convert $t to integer|j})
    | n => Ok((ts, Int(n)))
    }
  };

// let boolean_p: parser(literal);
let boolean_p = (toks: list(string)) =>
  switch (toks) {
  | [] => Error("boolean: empty input")
  | [t, ...ts] =>
    switch (t) {
    | "t" => Ok((ts, True))
    | "f" => Ok((ts, False))
    | _ => Error({j|boolean: cannot convert $t to boolean|j})
    }
  };

// this is gross
let rec literal_p = (l): parsed(literal) =>
  choice([integer_p, boolean_p, list_p, quotation_p], l)
and push_p = (l): parsed(word) =>
  (literal_p >>= (lit => pure(Push(lit))))(l)
and word_p = (l): parsed(word) => choice([builtin_p, push_p], l)
and words_p = (l): parsed(list(word)) => many(word_p, l)
and list_p = (l): parsed(literal) =>
  between(
    symbol_p("{"),
    symbol_p("}"),
    many(literal_p) >>= (lits => pure(List(lits))),
    l,
  )
and quotation_p = (l): parsed(literal) =>
  between(
    symbol_p("["),
    symbol_p("]"),
    words_p >>= (words => pure(Quotation(words))),
    l,
  );

let parse = (code: string): result(string, list(word)) =>
  lex(code)
  |> run_parser(words_p);
