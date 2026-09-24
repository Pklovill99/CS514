# Assignment 03 — solution

- `env/` — Question 1 environment ADT
- `lambda_expression/` — lecture parser extended with unary minus, comparisons, `&&` `||`, `let rec`, and closures
- `test/` — unit tests
- `bin/expr.ml` — REPL

```bash
dune build
dune runtest
dune exec ./bin/expr.exe -- 'let x = 10 in let f = fun y -> x + y in let x = 100 in f 5'
```

Expected value for that program: `15`.
