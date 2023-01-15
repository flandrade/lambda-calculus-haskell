# Lambda Calculus with Church Encodings

An implementation of lambda calculus and Church encodings in Haskell, with a focus on understanding the basic concepts and applications of these concepts.

# Lambda Calculus

Lambda calculus is a formal system for expressing computation. It consists of three types of expressions:

- **Variables**, denoted by $x$.
- **Abstraction**, denoted by $\lambda x. M$, which represents a function that takes an argument $x$ and returns the expression $M$.
- **Application**, denoted by $e_1$ $e_2$, which applies the expression $e_1$ to the argument $e_2$.

In this implementation, the datatype to represent the abstract syntax of the λ-calculus is the following:

```hs
data LambdaTerm
  = Variable String
  | Application LambdaTerm LambdaTerm
  | Lambda String LambdaTerm
```

In lambda calculus, a term is said to be **closed** if it has no free variables, and **open** if it does. The process of simplifying an expression by applying substitutions is called **beta-reduction**, and is defined by the rule $(\lambda x. e_1) e_2 \rightarrow e_1[e_2/x]$.

# Church Encodings

Church encodings are a way of representing data and operators in lambda calculus. This implementation includes encodings for several common data structures, including:

- **Booleans**, represented by `λx.λy.x` for true and `λx.λy.y` for false.
- **Numerals**, represented by `λf.λx.x` for 0, `λf.λx.f` x for 1, and so on.
- **Pairs**, represented by `λp.p true` for the first element, and `λp.p false` for the second element.

## Run

Install dependencies:

```
cabal install
```

Run the tests:

```
cabal test --test-show-details=direct
```
