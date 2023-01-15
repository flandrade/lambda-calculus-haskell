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
  deriving (Show, Eq)

-- Exercise 17. Based on the previous exercise, define a Haskell function that obtains the free variables
-- This function returns the set of free variables of a lambda LambdaTerm
freeVariables ::
  -- Lambda term to perform the operation
  LambdaTerm ->
  -- returns the set of free variables of a lambda LambdaTerm
  S.Set String
freeVariables (Variable var) = S.singleton var
freeVariables (Application lt1 lt2) = S.union (freeVariables lt1) (freeVariables lt2)
freeVariables (Lambda var lt) = S.delete var $ freeVariables lt

-- Exercise 18. Based on the previous exercises, define a Haskell function that implements capture
-- avoiding substitution.
-- Function to perform capture-avoiding substitution on a lambda term.
-- The function uses recursion to traverse the lambda term, checking the variable name at each level of the term,
-- and replacing it with the provided term if the variable name matches the variable being replaced. If the variable
-- is bound in a lambda abstraction, the function checks if it is captured by the replacement term, and if so, it creates
-- a fresh variable with a new name and perform the substitution on the modified term
substitute ::
  -- the lambda term to perform the substitution on
  LambdaTerm ->
  -- the variable name to be replaced
  String ->
  -- term that will be used as a replacement
  LambdaTerm ->
  -- returns the resulting term after the substitution
  LambdaTerm
-- if the current term is a variable and its name matches the variable to be replaced,
-- return the replacement term
substitute (Variable x) y sub = if x == y then sub else Variable x
-- if the current term is an application, recursively perform substitution on both subterms
substitute (Application t1 t2) y sub = Application (substitute t1 y sub) (substitute t2 y sub)
-- if the current term is a lambda abstraction:
substitute (Lambda x t1) y sub =
  -- if the variable bound in this lambda abstraction is the variable to be replaced,
  -- return the same lambda abstraction
  if x == y
    then Lambda x (substitute t1 y sub)
    else -- otherwise, check if the bound variable is captured by the replacement term

      if x `S.member` fvT2
        then -- if it is, create a fresh variable with a new name and perform the substitution on the modified term

          let x' = freshName (S.union fvT1 (S.union fvT2 (S.singleton x)))
              t1' = substitute t1 x (Variable x')
           in Lambda x' (substitute t1' y sub)
        else -- otherwise, perform the substitution on the term as normal
          Lambda x (substitute t1 y sub)
  where
    -- find the free variables in the current term and the replacement term
    fvT1 = freeVariables t1
    fvT2 = freeVariables sub

-- function to generate a fresh variable name that is not already in the set of free variables
freshName :: S.Set String -> String
freshName s = head $ filter (\x -> not (x `S.member` s)) $ map (\i -> "v" ++ show i) [(1 :: Integer) ..]

-- Exercise 19. Based on the previous exercises, define a Haskell function that implements β-reduction
-- (one step).
betaReduction :: LambdaTerm -> LambdaTerm
betaReduction (Application (Lambda v t) e) = substitute t v e
betaReduction x = x
