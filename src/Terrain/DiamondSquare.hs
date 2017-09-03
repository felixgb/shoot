module Terrain.DiamondSquare where

import Control.Monad (unless)
import Debug.Trace
import Data.List

import Graphics.GL.Core33

import Data.Array.IO
import System.Random

type Grid = IOArray (Int, Int) (GLfloat)

type Shape = ((Int, Int), [(Int, Int)])

maxSize = 512

inGrid :: Int -> (Int, Int) -> Bool
inGrid size (x, y) = x >= 0 && y >= 0 && x <= size && y <= size

gridMidpoints :: Int -> Int -> [Int]
gridMidpoints size level = [start, start + step .. size - 1]
  where
    toDivide = 2 ^ level
    start = size `div` toDivide
    step = (2 * start)

squareFromMidpoint :: Int -> (Int, Int) -> Shape
squareFromMidpoint size mp@(x, y) = (mp, ps)
  where
    ps =
      [ (x - size, y - size)
      , (x + size, y - size)
      , (x + size, y + size)
      , (x - size, y + size)
      ]

diamondFromMidpoint :: Int -> (Int, Int) -> Shape
diamondFromMidpoint size mp@(x, y) = (mp, ps)
  where
    ps =
      [ (x, y - size)
      , (x + size, y)
      , (x, y + size)
      , (x - size, y)
      ]

shapes ::  (Int -> (Int, Int) -> Shape) -> Int -> Int -> [Shape]
shapes func size level = map (func start) mps
  where
    toDivide = 2 ^ (level + 1)
    start = size `div` toDivide
    mps = [(x, y) | x <- (gridMidpoints size level), y <- (gridMidpoints size level)]

roughness = 0.08

drawShape :: Grid -> GLfloat -> Shape -> IO ()
drawShape grid offset (mp, ps) = do
  values <- mapM (readArray grid) $ filter (inGrid maxSize) ps
  let average = sum values / (fromIntegral $ length values)
  r <- randomRIO (-1, 1) :: IO GLfloat
  writeArray grid mp $ average + (r * roughness * offset)

diamondHack :: Int -> (Int, Int) -> [Shape]
diamondHack size mp = map (diamondFromMidpoint size) ps
  where (_, ps) = diamondFromMidpoint size mp

divide :: Grid -> Int -> IO ()
divide grid level = do
  let cubeSize = maxSize `div` (2 ^ level)
  unless (cubeSize < 1) $ do
    let mps = [(x, y) | x <- (gridMidpoints maxSize level), y <- (gridMidpoints maxSize level)]
    let squares = map (squareFromMidpoint cubeSize) mps
    let diamonds = concatMap (diamondHack cubeSize) mps
    let scale = (maxSize `div` level)
    mapM_ (drawShape grid $ fromIntegral scale) squares
    mapM_ (drawShape grid $ fromIntegral scale) diamonds
    divide grid (level + 1)

diamondSquare :: IO Grid
diamondSquare = do
  grid <- newArray ((0, 0), (maxSize, maxSize)) 0.0 :: IO Grid
  writeArray grid (0, 0) (-20.0)
  writeArray grid (maxSize, 0) 0.0
  writeArray grid (maxSize, maxSize) 0.0
  writeArray grid (0, maxSize) (-20.0)
  divide grid 1
  return grid
