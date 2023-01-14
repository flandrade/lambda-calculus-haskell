# Lambda calculus with Church encodings

Church encoding is a means of representing data and operators in the lambda calculus. Data structures
in this repository:

- Boolean. For instance, true is represented as `λx.λy.x`.
- Numerals. For instance, 0 is represented as `λf.λx.x`.
- Pairs

## Run

Run the tests:

```
cabal test --test-show-details=direct
```
