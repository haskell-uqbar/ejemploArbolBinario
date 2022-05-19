module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ordenamiento" $ do
    it "ordena lista de numeros" $ do
      ordenar [6,3,7,4,5] `shouldBe` [3,4,5,6,7]

