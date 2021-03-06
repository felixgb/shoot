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
      let expected = AABB (V3 (-1.0) (-1.0) (-1.0)) (V3 60.0 1.0 2.0)
      createAABB shape `shouldBe` expected

    it "intercects two BBs (false)" $ do
      let bb1 = AABB (V3 0 0 0) (V3 (-1) (-1) (-1))
      let bb2 = AABB (V3 1 1 1) (V3 2 2 2)
      (bb1 `insersectAABB` bb2) `shouldBe` False

    it "intercects two BBs (true)" $ do
      let bb1 = AABB (V3 (-1) (-1) (-1)) (V3 3 3 3)
      let bb2 = AABB (V3 (-3) (-3) (-3)) (V3 1 1 1)
      (bb1 `insersectAABB` bb2) `shouldBe` True

    it "intercects two BBs (true)" $ do
      let bb1 = AABB (V3 (-1) (-1) (-1)) (V3 3 3 3)
      let bb2 = AABB (V3 (-2) (-2) (-2)) (V3 (-1) (-1) (-1))
      (bb1 `insersectAABB` bb2) `shouldBe` True
