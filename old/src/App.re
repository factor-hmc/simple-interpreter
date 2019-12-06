open Js;

type snapshot = {
  input: string,
  stack: Lang.stack,
  output: string,
};

type model = {
  history: list(snapshot),
  current: snapshot,
};

type msg =
  | Input(string)
  | Enter
  | Nop;

let init: model = {
  history: [],
  current: {
    stack: [],
    input: "",
    output: "",
  },
};

external refocus: unit => unit = "refocus";

let update = (model: model): (msg => model) =>
  fun
  | Input(s) => {
      ...model,
      current: {
        ...model.current,
        input: s,
      },
    }
  | Enter => {
      let result = Eval.run(model.current.stack, model.current.input);
      Belt.Result.(
        switch (result) {
        | Ok(stack) => {
            current: {
              output: "",
              input: "",
              stack,
            },
            history: [model.current, ...model.history],
          }
        | Error(e) => {
            current: {
              output: e,
              input: model.current.input,
              stack: model.current.stack,
            },
            history: [model.current, ...model.history],
          }
        }
      );
    }
  | Nop => model;

let viewSnapshot = (~current: bool, snap: snapshot): Vdom.t(msg) =>
  Tea.Html.(
    div(
      [class'("snapshot")],
      (
        [div([class'("output")], [text(snap.output)])]
        @ (
          switch (snap.stack) {
          | [] => []
          | _ => [
              div(
                [class'("stack")],
                [
                  div([class'("title")], []),
                  ...snap.stack
                     ->Belt.List.mapReverse((lit: Lang.literal) =>
                         div([class'("item")], [text(Lang.repr(lit))])
                       ),
                ],
              ),
            ]
          }
        )
      )
      @ [
        div(
          [class'("prompt")],
          [
            span([class'("in")], []),
            current
              ? input'(
                  [
                    id("currentInput"),
                    class'("input"),
                    onInput(s => Input(s)),
                    value(snap.input),
                    on(
                      "keyup",
                      ~key="",
                      Tea.Json.Decoder.(
                        field("keyCode", int)
                        |> map(n => n == 13 ? Enter : Nop)
                      ),
                    ),
                  ],
                  [],
                )
              : span([class'("historyInput")], [text(snap.input)]),
          ],
        ),
      ],
    )
  );

let view = (model: model): Vdom.t(msg) => {
  Tea.Html.(
    div(
      [],
      model.history->Belt.List.mapReverse(viewSnapshot(~current=false))
      @ [viewSnapshot(~current=true, model.current)],
    )
  );
};

let main = Tea.App.beginnerProgram({model: init, update, view});
