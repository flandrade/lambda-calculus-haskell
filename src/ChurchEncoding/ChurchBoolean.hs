{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChurchEncoding.ChurchBoolean
  ( churchFalse,
    churchTrue,
    churchNeg,
    churchConj,
    churchDisj,
    boolChurch,
    boolUnchurch,
  )
where

type ChurchBoolean = forall r. r -> r -> r

-- | false = λx.λy.y
churchFalse :: ChurchBoolean
churchFalse _ y = y

-- | true = λx.λy.x
churchTrue :: ChurchBoolean
churchTrue x _ = x

-- | conditional = λp.λx.λy.p x y
churchCond :: ChurchBoolean -> ChurchBoolean -> ChurchBoolean -> ChurchBoolean
churchCond predicate = predicate

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
boolUnchurch :: (Bool -> Bool -> a) -> a
boolUnchurch = (\a b c -> c a b) True False

-- | Convert boolean to
boolChurch :: Bool -> ChurchBoolean
boolChurch b = if b then churchTrue else churchFalse
