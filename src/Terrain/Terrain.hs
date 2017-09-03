module Terrain.Terrain where

import Control.Monad (forM_)
import Data.Array.IO
import Data.List (unfoldr)

import Graphics.GL.Core33
import Terrain.DiamondSquare

import Parser.ObjectParser

type Terrain = IOArray (Int, Int) (GLfloat, GLfloat, GLfloat)

mapScale :: GLfloat
mapScale = 1.0

triangleStrip :: GLuint -> GLuint -> [(GLuint, GLuint, GLuint)]
triangleStrip len y = concat $ unfoldr build 0
  where
    x = len + 1
    build i
      | i > (len - 1) = Nothing
      | otherwise = Just ((tri (i + (y * x))), i + 1)
    tri i = [(i, i + 1, i + x), (i + 1, i + x, i + 1 + x)]

triangleGrid :: GLuint -> [(GLuint, GLuint, GLuint)]
triangleGrid size = concatMap (triangleStrip size) [0..size - 1]

tuple3Flatten :: (a, a, a) -> [a]
tuple3Flatten (a, b, c) = [a, b, c]

buildTerrain :: IO Terrain
buildTerrain = do
  terrain <- newArray ((0, 0), (maxSize, maxSize)) (0.0, 0.0, 0.0) :: IO Terrain
  grid <- diamondSquare
  forM_ [0..maxSize] $ \y -> do
    forM_ [0..maxSize] $ \x -> do
      val <- readArray grid (x, y)
      writeArray terrain (x, y) ((fromIntegral x) * mapScale, val, (fromIntegral y) * mapScale)
  return terrain

terrainToObj :: Terrain -> IO Object
terrainToObj terrain = do
   vs <- getElems terrain
   let vns = map (\_ -> (0, 1, 0)) vs
   let is = triangleGrid (fromIntegral maxSize)
   return $ Object (concatMap tuple3Flatten vs) [] (concatMap tuple3Flatten vns) (concatMap tuple3Flatten is)

flatTerrain :: IO Object
flatTerrain = buildTerrain >>= terrainToObj
