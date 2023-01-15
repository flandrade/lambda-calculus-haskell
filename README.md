# Lambda Calculus with Church Encodings

An attempt to implement lambda calculus and church encodings with Haskell

# Lambda Calculus

There are three kinds of expressions (also called terms) in the pure lambda calculus:

- $x$: variable
- $\lambda x. M$: abstraction
- $e_1$ $e_2$: applications

Notes:

- A term is closed if it has no free variables; otherwise it is open.
- Beta-reduction is defined by: $(\lambda x. e_1) e_2 \rightarrow e_1[e_2/x]$

# Church Encodings

Church encoding is a means of representing data and operators in the lambda calculus. Data structures
in this repository:

- Boolean. For instance, true is represented as `λx.λy.x`.
- Numerals. For instance, 0 is represented as `λf.λx.x`.
- Pairs

## Run

Install dependencies:

```
cabal install
```

Run the tests:

```
cabal test --test-show-details=direct
```
