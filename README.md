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

For instance, the term: `(λx.λy.x y) y` is represented as follows:

```hs
(Lambda "x" (Lambda "y" (Application (Variable "x") (Variable "y")))) (Variable "y")
```

In lambda calculus, a term is said to be **closed** if it has no free variables, and **open** if it does. The process of simplifying an expression by applying substitutions is called **beta-reduction**, and is defined by the rule $(\lambda x. e_1) e_2 \rightarrow e_1[e_2/x]$. This implementation utilizes these concepts to implement two algorithms:

- **Capture-avoiding substitution**, a method for replacing variables in a lambda term without altering the meaning of the term.
- **Beta reduction**, a process for simplifying expressions by applying a single step of beta-reduction.

# Church Encodings

Church encodings are a way of representing data and operators in lambda calculus. This implementation includes encodings for several common data structures, including:

- **Booleans**, represented by `λx.λy.x` for true and `λx.λy.y` for false.

  ```haskell
  newtype ChurchBoolean = ChurchBoolean (forall r. r -> r -> r)

  -- false = λx.λy.y
  churchFalse :: ChurchBoolean
  churchFalse = ChurchBoolean $ \_ y -> y

  -- true = λx.λy.x
  churchTrue :: ChurchBoolean
  churchTrue = ChurchBoolean $ \x \_ -> x

  -- Convert Church Boolean to boolean
  boolUnchurch :: ChurchBoolean -> Bool
  boolUnchurch (ChurchBoolean b) = b True False

  -- Convert boolean to Church Boolean
  boolChurch :: Bool -> ChurchBoolean
  boolChurch b = if b then churchTrue else churchFalse
  ```

- **Numerals**, represented by `λf.λx.x` for 0, `λf.λx.f` x for 1, and so on.

  ```haskell
  newtype ChurchNum = ChurchNum (forall r. (r -> r) -> r -> r)

  -- 0 = λf.λx.x
  churchZero :: ChurchNum
  churchZero = ChurchNum $ \_ x -> x

  -- Represent any numeral as Church numeral
  -- num n = λf.λx.f (numChurch (n-1) f x)
  numChurch :: Integer -> ChurchNum
  numChurch 0 = ChurchNum $ \_ x -> x
  numChurch n = successor $ numChurch (n - 1)
  ```

- **Pairs**, represented by `λp.p true` for the first element, and `λp.p false` for the second element.

## Run examples

Install dependencies:

```
cabal install
```

Run the tests:

```
cabal test --test-show-details=direct
```

## Acknowledgment

Assignment from "Theory of Programming Languages" at Universidad Complutense de Madrid and Universidad Politécnica de Madrid. Prof. Julio Mariño.
