module LambdaCalculus.LambdaCalculusSpec (spec) where

import qualified Data.Set as S
import LambdaCalculus.LambdaCalculus
import Test.Hspec

spec :: Spec
spec = do
  describe "freeVariables" $ do
    it "returns the free variable of a lambda term - variable" $ do
      freeVariables (Variable "x") `shouldBe` S.singleton "x"
    it "returns the free variable of a lambda term - application" $ do
      freeVariables (Application (Variable "x") (Variable "y")) `shouldBe` S.union (S.singleton "x") (S.singleton "y")
    it "returns the free variable of a lambda term - abstraction" $ do
      freeVariables (Lambda "y" (Application (Variable "y") (Variable "x"))) `shouldBe` S.singleton "x"
    it "returns the free variable of a lambda term - abstraction" $ do
      freeVariables (Lambda "y" (Variable "y")) `shouldBe` S.empty

  describe "substitute" $ do
    it "replaces a variable in a term" $ do
      let t = Variable "x"
      let e = Variable "y"
      substitute t "x" e `shouldBe` e
    it "replaces a variable in a nested term" $ do
      let t = Lambda "x" (Variable "x")
      let e = Variable "y"
      substitute t "x" e `shouldBe` Lambda "x" e
    it "replaces a variable in a complex term" $ do
      let t = Application (Lambda "x" (Variable "x")) (Variable "y")
      let e = Variable "z"
      substitute t "y" e `shouldBe` (Application (Lambda "x" (Variable "x")) e)
    it "avoids variable capture" $ do
      let t = Lambda "x" (Application (Variable "x") (Variable "y"))
      let e = Variable "x"
      substitute t "y" e `shouldBe` (Lambda "v1" (Application (Variable "v1") (Variable "x")))

  describe "betaReduction" $ do
    it "reduces a lambda term in an step" $ do
      let t = Application (Lambda "x" (Variable "x")) (Variable "y")
      betaReduction t `shouldBe` Variable "y"
