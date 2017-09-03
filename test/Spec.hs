import Test.Hspec
import Test.QuickCheck
import Data.List

import Graphics.GL.Core33
import Linear

import Terrain.DiamondSquare
import Terrain.Terrain
import Entity.Collision

main :: IO ()
main = hspec $ do

  describe "Diamond Sqaure" $ do
    it "gets the midpoint of a square" $ do
      (gridMidpoints 128 1) `shouldBe` [64]
      (gridMidpoints 8 2) `shouldBe` [2, 6]
      (gridMidpoints 16 3) `shouldBe` [2, 6, 10, 14]

  describe "Entry Collision" $ do
    it "Gets the bounding box" $ do
      let shape = [1, 1, 1, -1, -1, -1, 60, 0, 2]
      let expected = AABB (-1.0, -1.0, -1.0) (60.0, 1.0, 2.0)
      createAABB shape `shouldBe` expected

    it "intercects two BBs (false)" $ do
      let bb1 = AABB (0, 0, 0) (-1, -1, -1)
      let bb2 = AABB (1, 1, 1) (2, 2, 2)
      (bb1 `insersectAABB` bb2) `shouldBe` False

    it "intercects two BBs (true)" $ do
      let bb1 = AABB (-1, -1, -1) (3, 3, 3)
      let bb2 = AABB (-3, -3, -3) (1, 1, 1)
      (bb1 `insersectAABB` bb2) `shouldBe` True

    it "intercects two BBs (true)" $ do
      let bb1 = AABB (-1, -1, -1) (3, 3, 3)
      let bb2 = AABB (-2, -2, -2) (-1, -1, -1)
      (bb1 `insersectAABB` bb2) `shouldBe` True
