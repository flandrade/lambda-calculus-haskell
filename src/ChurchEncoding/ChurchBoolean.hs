{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module ChurchEncoding.ChurchBoolean
  ( ChurchBoolean,
    churchFalse,
    churchTrue,
    churchNeg,
    churchConj,
    churchDisj,
    boolChurch,
    boolUnchurch,
    unpackBoolean,
  )
where

newtype ChurchBoolean = ChurchBoolean (forall r. r -> r -> r)

unpackBoolean :: ChurchBoolean -> r -> r -> r
unpackBoolean (ChurchBoolean n) = n

-- | false = λx.λy.y
churchFalse :: ChurchBoolean
churchFalse = ChurchBoolean $ \_ y -> y

-- | true = λx.λy.x
churchTrue :: ChurchBoolean
churchTrue = ChurchBoolean $ \x _ -> x

-- | conditional = λp.λx.λy.p x y
churchCond :: ChurchBoolean -> ChurchBoolean -> ChurchBoolean -> ChurchBoolean
churchCond (ChurchBoolean predicate) = predicate

-- | neg = λx.churchCond λx false true
churchNeg :: ChurchBoolean -> ChurchBoolean
churchNeg value = churchCond value churchFalse churchTrue

-- | conj = λx.churchCond λx false true
churchConj :: ChurchBoolean -> ChurchBoolean -> ChurchBoolean
churchConj x y = churchCond x y churchFalse

-- | conj = λx.churchCond λx false true
churchDisj :: ChurchBoolean -> ChurchBoolean -> ChurchBoolean
churchDisj x = churchCond x churchTrue

-- | Convert Church Boolean to boolean
boolUnchurch :: ChurchBoolean -> Bool
boolUnchurch (ChurchBoolean b) = b True False

-- | Convert boolean to Church Boolean
boolChurch :: Bool -> ChurchBoolean
boolChurch b = if b then churchTrue else churchFalse
