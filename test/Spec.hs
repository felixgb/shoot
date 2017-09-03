import Test.Hspec
import Test.QuickCheck

import Data.List

import Terrain.DiamondSquare

main :: IO ()
main = hspec $ do

  describe "Diamond Sqaure" $ do
    it "gets the midpoint of a square" $ do
      (gridMidpoints 128 1) `shouldBe` [64]
      (gridMidpoints 8 2) `shouldBe` [2, 6]
      (gridMidpoints 16 3) `shouldBe` [2, 6, 10, 14]
