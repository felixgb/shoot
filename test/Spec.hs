import Test.Hspec
import Test.QuickCheck

import Data.List

import Terrain.DiamondSquare
import Terrain.Terrain

import Linear

main :: IO ()
main = hspec $ do

  describe "Diamond Sqaure" $ do
    it "gets the midpoint of a square" $ do
      (gridMidpoints 128 1) `shouldBe` [64]
      (gridMidpoints 8 2) `shouldBe` [2, 6]
      (gridMidpoints 16 3) `shouldBe` [2, 6, 10, 14]

  describe "Terrain" $ do
    it "Gets the surface normal" $ do
      let v1 = V3 0 0 0
      let v2 = V3 1 0 1
      let v3 = V3 0 0 1
      surfaceNormal v1 v2 v3 `shouldBe` V3 0 1 0
