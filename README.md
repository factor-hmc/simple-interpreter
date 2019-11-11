# Examples

```
>>> eval_words([Int(7)], 
  [ Dup
  , Push(Quotation([Dup, Push(Int(1)), Eq, Push(False), Eq]))
  , Push(Quotation([Push(Int(1)), Sub, Dup, Rot, Mul, Swap]))
  , While, Drop])
- : stack = [Int(5040)]
```

# Boilerplate below
# Basic Reason Template

Hello! This project allows you to quickly get started with Reason and BuckleScript. If you wanted a more sophisticated version, try the `react` template (`bsb -theme react -init .`).

# Build
```
npm run build
```

# Build + Watch

```
npm run start
```


# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically
