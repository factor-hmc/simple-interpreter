type literal =
  | Int(int)
  | True
  | False
  | String(string)
  | List(list(literal))
  | Quotation(list(word))

and stack = list(literal)

and word =
  | Push(literal)
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
| While
| Clear;

let rec reprWord: word => string =
  fun
  | Push(lit) => repr(lit)
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
  | Eq => "="
  | If => "if"
  | Dup => "dup"
  | Swap => "swap"
  | Rot => "rot"
  | Drop => "drop"
  | While => "while"
| Clear => "clear"
and repr: literal => string =
  fun
  | Int(n) => string_of_int(n)
  | True => "t"
  | False => "f"
  | String(s) => "\"" ++ s ++ "\""
  | List(l) =>
    "{ "
    ++ l->Belt.List.map(repr)->Belt.List.toArray->Js.Array2.joinWith(" ")
    ++ " }"
  | Quotation(ws) =>
    "[ "
    ++ ws
       ->Belt.List.map(reprWord)
       ->Belt.List.toArray
       ->Js.Array2.joinWith(" ")
    ++ " ]";

let stringOfLiteral = (literal: literal): string =>
  switch (literal) {
  | Int(x) => "Int(" ++ string_of_int(x) ++ ")"
  | True => "True"
  | False => "False"
  | String(_) => "String"
  | List(_) => "List"
  | Quotation(_) => "Quotation"
  };

let print_literal = (lit: literal): unit =>
  print_endline(stringOfLiteral(lit));

let print_stack = (stack: stack): unit =>
  stack |> List.iter((elem: literal) => print_literal(elem));

let to_bool = (literal: literal): bool =>
  switch (literal) {
  | False => false
  | _ => true
  };
