{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module ChurchEncoding.ChurchNumeralSpec (spec) where

import ChurchEncoding.ChurchBoolean (boolUnchurch, churchFalse, churchTrue)
import ChurchEncoding.ChurchNumeral
import Test.Hspec

spec :: Spec
spec = do
  describe "numChurch and numUnchurch" $ do
    it "converts integer to Church numeral and vice-versa" $ do
      numUnchurch (numChurch 3) `shouldBe` 3
  describe "churchAddition" $ do
    it "adds a Church numeral to another Church numeral" $ do
      numUnchurch (churchAddition (numChurch 3) (numChurch 7)) `shouldBe` 10
  describe "churchMul" $ do
    it "multiplies a Church numeral with another Church numeral" $ do
      numUnchurch (churchMul (numChurch 3) (numChurch 7)) `shouldBe` 21
  describe "predecessor" $ do
    it "returns the predecessor of a Church numeral" $ do
      numUnchurch (predecessor (numChurch 7)) `shouldBe` 6
  describe "churchSub" $ do
    it "subtracts a Church numeral from another Church numeral" $ do
      numUnchurch (churchSub (numChurch 7) (numChurch 3)) `shouldBe` 4
  describe "isEven" $ do
    it "returns whether the Church numeral is even or not" $ do
      boolUnchurch (isEven (numChurch 4)) `shouldBe` boolUnchurch churchTrue
      boolUnchurch (isEven (numChurch 3)) `shouldBe` boolUnchurch churchFalse
