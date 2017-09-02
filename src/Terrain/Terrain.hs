module Terrain.Terrain where

import Control.Monad.ST
import Control.Monad (forM_)
import Data.Array.IO
import Data.Array
import Data.List (unfoldr)

import Graphics.GL.Core33

import Parser.ObjectParser

type Terrain = IOArray (Int, Int) (GLfloat, GLfloat, GLfloat)

newGrid :: Int -> Int -> IO Terrain
newGrid xsize zsize = newArray ((0, 0), (xsize, zsize)) (0.0, 0.0, 0.0)

getAt = do
  grid <- newGrid 10 10
  e <- readArray grid (0, 5)
  return e

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

tuple3Flatten (a, b, c) = [a, b, c]

buildTerrain :: IO Terrain
buildTerrain = do
  terrain <- newGrid 10 10
  forM_ [0..9] $ \y -> do
    forM_ [0..9] $ \x -> do
      writeArray terrain (x, y) ((fromIntegral x) * scale, 0.0, (fromIntegral y) * scale)
  return terrain

terrainToObj :: Terrain -> IO Object
terrainToObj terrain = do
   vs <- getElems terrain
   let vns = map (\_ -> (0, 1, 0)) vs
   let is = triangleGrid 10
   return $ Object (concatMap tuple3Flatten vs) [] (concatMap tuple3Flatten vns) (concatMap tuple3Flatten is)

flatTerrain :: IO Object
flatTerrain = buildTerrain >>= terrainToObj

scale :: GLfloat
scale = 20.0

to2d :: [a] -> [[a]]
to2d [] = []
to2d xs = xh : to2d xt
  where (xh, xt) = splitAt 11 xs

printList :: (Show a) =>  [(a, a, a)] -> IO ()
printList ls = putStrLn (concatMap (\(_, _, e) -> (show e) ++ "\t") ls)

printArray :: IO ()
printArray = do
  arr <- newGrid 10 10
  elems <- getElems arr
  mapM_ printList (to2d elems)
