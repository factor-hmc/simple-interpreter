# Installation
Run
```
$ npm install
```
in the root of the directory to install the necessary files.

Install parcel.

```
$ yarn global add parcel-bundler
```
or
```
$ npm install -g parcel-bundler
```

# Building

```
$ npm run build
```
for a single build of the code or
```
$ npm run start
```
for a continuous build on filesave.

```
$ parcel index.html
```

to build the "application" (currently just an empty page that serves the js output).

# Notes

Parsing _could_ work properly except OCaml doesn't allow recursive value
definitions!! So I've removed the mutually recursive parts of the parser that
cause type errors. I will fix this eventually.

Right now the `result` type is the opposite order of `Belt.Result.t` since
that's the convention I was used to. I export a `result` constructor in `Utils`
for convenience which is flipped. It'll get fixed eventually...

# Examples

```
>>> eval_words([Int(7)], 
  [ Dup
  , Push(Quotation([Dup, Push(Int(1)), Eq, Push(False), Eq]))
  , Push(Quotation([Push(Int(1)), Sub, Dup, Rot, Mul, Swap]))
  , While, Drop])
- : stack = [Int(5040)]
```
