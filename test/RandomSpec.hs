module RandomSpec where

import Random (randomNM)
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

stdGen = mkStdGen 0

spec =
  describe "randomNM" $ do
    it "should support negative numbers as height and width" $ do
      length (fst (randomNM (-1) (-1) stdGen :: ([[Bool]], StdGen))) `shouldBe` 0
      length (head (fst (randomNM maxBound (-1) stdGen :: ([[Bool]], StdGen)))) `shouldBe` 0
    prop "should support arbitrary numbers as height and width" $
      \(Positive n) (Positive m) -> length (fst (randomNM n m stdGen :: ([[Bool]], StdGen))) `shouldBe` n
