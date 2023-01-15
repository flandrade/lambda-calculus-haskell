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
