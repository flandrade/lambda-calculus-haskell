{-# LANGUAGE BlockArguments #-}

module ChurchEncoding.ChurchBooleanSpec (spec) where

import ChurchEncoding.ChurchBoolean
import Test.Hspec

spec :: Spec
spec = do
  describe "boolChurch and boolUnchurch" $ do
    it "converts boolean to Church boolean and vice-versa" $ do
      boolUnchurch (boolChurch True) `shouldBe` True
      boolUnchurch (boolChurch False) `shouldBe` False
  describe "churchNeg" $ do
    it "negates a church boolean" $ do
      boolUnchurch (churchNeg churchTrue) `shouldBe` boolUnchurch churchFalse
  describe "churchConj" $ do
    it "returns true when applying conjunction to true and true" $ do
      boolUnchurch (churchConj churchTrue churchTrue) `shouldBe` boolUnchurch churchTrue
    it "returns false when applying conjunction to true and false" $ do
      boolUnchurch (churchConj churchTrue churchFalse) `shouldBe` boolUnchurch churchFalse
    it "returns false when applying conjunction to false and false" $ do
      boolUnchurch (churchConj churchFalse churchFalse) `shouldBe` boolUnchurch churchFalse
  describe "churchDisj" $ do
    it "returns true when applying disjunction to true and true" $ do
      boolUnchurch (churchDisj churchTrue churchTrue) `shouldBe` boolUnchurch churchTrue
    it "returns true when applying disjunction to true and false" $ do
      boolUnchurch (churchDisj churchTrue churchFalse) `shouldBe` boolUnchurch churchTrue
    it "returns false when applying disjunction to false and false" $ do
      boolUnchurch (churchDisj churchFalse churchFalse) `shouldBe` boolUnchurch churchFalse
