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
