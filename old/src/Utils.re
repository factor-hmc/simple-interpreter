open Belt.Result;

type result('error, 'ok) = Belt.Result.t('ok, 'error);

let result_of_option =
    (failure: 'failure, opt: option('ok)): result('failure, 'ok) =>
  switch (opt) {
  | None => Error(failure)
  | Some(ok) => Ok(ok)
  };

let rec lookup = (key: 'a, assoc: list(('a, 'b))): option('b) =>
  switch (assoc) {
  | [] => None
  | [(a, b), ..._rest] when a == key => Some(b)
  | [_, ...rest] => lookup(key, rest)
  };
