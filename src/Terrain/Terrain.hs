module Terrain.Terrain where

import Control.Monad (forM_)
import Data.Array.IO
import Data.List (unfoldr)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Graphics.GL.Core33
import Linear

import Parser.ObjectParser
import Terrain.DiamondSquare

type Vertex = V3 GLfloat

type Face = V3 GLuint

type Terrain = IOArray (Int, Int) Vertex

mapScale :: GLfloat
mapScale = 10.0

triangleStrip :: GLuint -> GLuint -> [Face]
triangleStrip len y = concat $ unfoldr build 0
  where
    x = len + 1
    build i
      | i > (len - 1) = Nothing
      | otherwise = Just (tri (i + y * x), i + 1)
    tri i = [V3 i (i + x) (i + 1), V3 (i + 1) (i + x) (i + 1 + x)]

triangleGrid :: GLuint -> [Face]
triangleGrid size = concatMap (triangleStrip size) [0..size - 1]

vecFlatten :: V3 a -> [a]
vecFlatten (V3 a b c) = [a, b, c]

buildTerrain :: IO Terrain
buildTerrain = do
  terrain <- newArray ((0, 0), (maxSize, maxSize)) (V3 0.0 0.0 0.0) :: IO Terrain
  grid <- diamondSquare
  forM_ [0..maxSize] $ \y -> do
    forM_ [0..maxSize] $ \x -> do
      val <- readArray grid (x, y)
      writeArray terrain (x, y) $ V3 ((fromIntegral x) * mapScale) val ((fromIntegral y) * mapScale)
  return terrain

terrainToObj :: Terrain -> IO Object
terrainToObj terrain = do
   vs <- V.fromList <$> getElems terrain
   vns <- VM.new (V.length vs)
   let is = triangleGrid (fromIntegral maxSize)
   -- compute vector normals
   mapM_ (writeSurfaceNormal vs vns) is
   vnsI <- V.freeze vns
   return $ Object (concatMap vecFlatten $ V.toList vs) [] (concatMap vecFlatten $ V.toList vnsI) (concatMap vecFlatten is)

flatTerrain :: IO Object
flatTerrain = buildTerrain >>= terrainToObj

writeSurfaceNormal :: V.Vector Vertex -> VM.IOVector Vertex -> Face -> IO ()
writeSurfaceNormal vs vns (V3 i1 i2 i3) = do
  let v1 = vs V.! (fromIntegral i1)
  let v2 = vs V.! (fromIntegral i2)
  let v3 = vs V.! (fromIntegral i3)
  let u = v1 - v3
  let v = v1 - v2
  let out = normalize $ u `cross` v
  VM.write vns (fromIntegral i1) out
  VM.write vns (fromIntegral i2) out
  VM.write vns (fromIntegral i3) out

surfaceNormal :: V3 Vertex -> V3 Vertex -> V3 Vertex -> V3 Vertex
surfaceNormal a b c = normalize out
  where
    u = a - b
    v = a - c
    out = normalize $ u `cross` v
