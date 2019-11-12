type either('failure, 'ok)
  = Failure('failure)
  | Result('ok);

let either_of_option(failure: 'failure, opt: option('ok)): either('failure, 'ok) =
  switch(opt) {
  | None     => Failure(failure);
  | Some(ok) => Result(ok);
  };

let rec lookup(key: 'a, assoc: list(('a, 'b))): option('b) =
  switch(assoc){
  | [] => None;
  | [(a, b), ...rest] when a == key => Some(b);
  | [_, ...rest] => lookup(key, rest);
  };
