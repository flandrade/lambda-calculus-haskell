-- |
-- | Author      :  Fernanda Lucia Andrade Guanoquiza
-- | Date        :  2023-01-16
-- |
-- | Lambda Calculus (see LambdaCalculusSpec for tests)
module LambdaCalculus.LambdaCalculus where

import qualified Data.Set as S

-- Exercise 16. Define a Haskell datatype to represent the abstract syntax of the λ-calculus
-- Lambda expressions can have the following syntax:
-- <exp> :: = variable
--          | <exp> <exp> (application)
--          | λ <variable> <exp> (abstraction)
data LambdaTerm
  = Variable String
  | Application LambdaTerm LambdaTerm
  | Lambda String LambdaTerm

-- Exercise 17. Based on the previous exercise, define a Haskell function that obtains the free variables
-- This function returns the set of free variables of a lambda term
freeVariables :: LambdaTerm -> S.Set String
freeVariables (Variable var) = S.singleton var
freeVariables (Application lt1 lt2) = S.union (freeVariables lt1) (freeVariables lt2)
freeVariables (Lambda var lt) = S.delete var $ freeVariables lt

-- Exercise 18. Based on the previous exercises, define a Haskell function that implements capture
-- avoiding substitution.
