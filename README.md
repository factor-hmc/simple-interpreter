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

Right now the `result` type is the opposite order of `Belt.Result.t` since
that's the convention I was used to. I export a `result` constructor in `Utils`
for convenience which is flipped. It'll get fixed eventually...

# Examples

Typing literals pushes them onto the stack
```
  1 2
-- Data stack:
1
2
```

Simple operations
```
  +
-- Data stack:
3
  1 -
-- Data stack:
2
  5 *
-- Data stack:
10
  dup
-- Data stack:
10
10
  drop
-- Data stack:
10
  dup dup dup
-- Data stack:
10
10
10
10
  clear
```

If
```
  10 t [ 1 + ] [ 1 - ] if
-- Data stack:
11
  10 f [ 1 + ] [ 1 - ] if
-- Data stack:
9
```

Factorial
```
  7 dup [ dup 1 = f = ] [ 1 - dup rot * swap ] while drop
-- Data stack:
5040
```
