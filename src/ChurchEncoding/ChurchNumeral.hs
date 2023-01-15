{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}

-- |
-- | Author      :  Fernanda Lucia Andrade Guanoquiza
-- | Date        :  2023-01-16
-- |
-- | Church Numerals (see ChurchNumeralSpec to check the solutions of exercises 7, 8, 10, 11, 12)
-- | Exercise 15. Analogously to the previous exercise, define Haskell functions to convert between Haskell
-- | and Church natural numbers, and check the correctness of your solutions to exercises 7, 8, 10, 11 and 12
module ChurchEncoding.ChurchNumeral
  ( numChurch,
    numUnchurch,
    churchAddition,
    churchMul,
    pair,
    fist,
    send,
    predecessor,
    churchSub,
    isEven,
  )
where

import ChurchEncoding.ChurchBoolean (ChurchBoolean, churchNeg, churchTrue)

newtype ChurchNum = ChurchNum (forall r. (r -> r) -> r -> r)

-- | 0 = λf.λx.x
churchZero :: ChurchNum
churchZero = ChurchNum $ \_ x -> x

-- successor = λn.λf.λx.f (n f x)
successor :: ChurchNum -> ChurchNum
successor (ChurchNum n) = ChurchNum $ \f x -> f (n f x)

-- | Represent any numeral as Church numeral
-- num n = λf.λx.f (numChurch (n-1) f x)
numChurch :: Integer -> ChurchNum
numChurch 0 = ChurchNum $ \_ x -> x
numChurch n = successor $ numChurch (n - 1)

-- | Convert church numeral to integer
-- λa.a (λb.b+1) 0
numUnchurch :: ChurchNum -> Integer
numUnchurch (ChurchNum a) = a (\b -> b + 1) 0

-- | addition = λm.λn.λf.λx.m f (n f x)
churchAddition :: ChurchNum -> ChurchNum -> ChurchNum
churchAddition (ChurchNum n) (ChurchNum m) = ChurchNum $ \f x -> m f (n f x)

-- | multiplication = λm.λn.λf.m (n f)
churchMul :: ChurchNum -> ChurchNum -> ChurchNum
churchMul (ChurchNum n) (ChurchNum m) = ChurchNum $ \f -> m (n f)

-- | isEvent = λc.c(λb NEG b) TRUE
isEven :: ChurchNum -> ChurchBoolean
isEven (ChurchNum c) = c (\b -> churchNeg b) churchTrue

-- CHURCH PAIRS

newtype ChurchPair a b = ChurchPair (forall c. (a -> b -> c) -> c)

-- pair = λx.λy.λz.z x y
pair :: a -> b -> ChurchPair a b
pair a b = ChurchPair $ \p -> p a b

-- FST = λp.p TRUE
fist :: ChurchPair a b -> a
fist (ChurchPair p) = p (\x _ -> x)

-- SND λp.p FALSE
send :: ChurchPair a b -> b
send (ChurchPair p) = p (\_ y -> y)

-- | predecessor = λn . fist (n next initial)
predecessor :: ChurchNum -> ChurchNum
predecessor (ChurchNum n) = fist (n next initial)
  where
    next p = pair (send p) (successor $ send p)
    initial = pair churchZero churchZero

-- | substraction = λn.λm.m predecessor n
churchSub :: ChurchNum -> ChurchNum -> ChurchNum
churchSub n (ChurchNum m) = m predecessor n

-- | half = λn send (n next initial)
-- churchHalf :: ChurchNum -> ChurchNum
-- churchHalf (ChurchNum n) = send (n next initial)
--   where
--     next p = fist p (\x _ -> x, successor (send p)) (\_ y -> y, send p)
--     initial = pair (\_ y -> y) churchZero
