{-# LANGUAGE OverloadedStrings, UnboxedTuples, MagicHash #-}

module Lib where

import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Monad.Loops
import Debug.Trace
import Data.Array.IO
import Data.ByteString.Internal (ByteString(..))
import Data.Array.IO.Internals  (IOUArray(..))
import Data.Array.Base          (STUArray(..))
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS

import Numeric.LinearAlgebra.Data hiding (size)
import Numeric.LinearAlgebra hiding (size)

import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import GHC.IO             (IO(..))
import GHC.Exts           (copyMutableByteArrayToAddr#, Ptr(..), Int(..))

type Buffer = IOUArray Int Word8

size = 1000;
numBytes = size * size

-- Thanks to https://stackoverflow.com/questions/45784663/iouarray-to-bytesring-as-quickly-as-possible
arrayToBS :: Buffer -> IO ByteString
arrayToBS (IOUArray (STUArray _ _ n@(I# n') mutByteArr)) = do
  bytes <- mallocForeignPtrBytes n
  withForeignPtr bytes $ \(Ptr addr) -> IO $ \state ->
    (# copyMutableByteArrayToAddr# mutByteArr 0# addr n' state, () #)
  pure (PS bytes 0 n)

newBuffer :: IO (IOUArray Int Word8)
newBuffer = newArray (0, numBytes + 1) 0

drawPoint :: Buffer -> (Int, Int) -> Color -> IO ()
drawPoint arr (x, y) (Color r g b a)
  | (x < 0 || x > size - 1) || (y < 0 || y > size - 1) = return ()
  | otherwise = do
    writeArray arr (base + 0) byte
    where
      base = (size * y) + x
      byte = foldl (\b c -> (shift b 2) .|. c) 0 [r, g, b, a]

type Point = (Int, Int)

bresenhamn :: Buffer -> Bool -> Point -> Point -> IO [Point]
bresenhamn buf steep (x0, y0) (x1, y1)
  | abs (x0 - x1) < abs (y0 - y1) = bresenhamn buf True (y0, x0) (y1, x1)
  | x0 > x1                       = bresenhamn buf steep (x1, y1) (x0, y0)
  | otherwise = do
    let dx = x1 - x0
    let dy = y1 - y0
    let derr = (abs dy) * 2
    let err = 0
    let negy = if y1 > y0 then 1 else -1
    bres x0 y0 err derr dx negy []
    where
      bres x y err derr dx negy acc
        | x > x1   = return acc
        | otherwise = do
          let p = if steep then (y, x) else (x, y)
          drawPoint buf p red
          let err' = err + derr
          let (err'', y') = if err' > dx then (err' - dx * 2, y + negy) else (err', y)
          bres (x + 1) y' err'' derr dx negy $ p : acc

type Line = (Vector Double, Vector Double)
-- | World space:
-- The space in which points are defined in the 3D scene.

-- | Camera space:
-- The space in which points are defined with respect to the
-- camera. In order to convert points from world to camera space, we need to
-- multiple points in world space by the inverse of the world to camera matrix.

-- cameraToWorld :: Matrix Double
-- cameraToWorld = (4 >< 4)
--   [ 0.871214  , 0         , -0.490904 , 0
--   , -0.192902 , 0.919559  , -0.342346 , 0
--   , 0.451415  , 0.392953  , 0.801132  , 0  -- Z axis, smaller is further away
--   , 14.777467 , 29.361945 , 27.993464 , 1
--   ]

yRot :: Double -> Matrix Double
yRot d = (4 >< 4)
  [ cos d , 0 , - (sin d) , 0
  , 0     , 1 , 0         , 0
  , sin d , 0 , cos d     , 0
  , 0     , 0 , 0         , 1
  ]

cameraToWorld :: Matrix Double
cameraToWorld = (4 >< 4)
  [ 0.5  , 0.0  , -0.0  , 0
  , -0.0  , 0.5  , -0.0  , 0
  , 0.0  , 0.0  , 0.5  , 0  -- Z axis , smaller is further away
  , 5 , 5 , 50 , 1
  ]

worldToCamera = inv cameraToWorld

-- In order to computer a point P' (coordinates of point P projected to canvas)
-- P must be defined with respect to the camera coordinate system. However,
-- points from the 3d scene are originally defined with respect to the world
-- coordinate system.
--
-- Therefore, the first operation we need to apply to points before projecting
-- them onto the canvas is to convert them from world to camera space.
--
-- P_camera = P_world * M_world_to_camera
--
-- (M_world_to_camera is the inverse of M_camera_to_world)
-- Once points are in camera space perspective projection can be applied.

-- | Perspective Divide:
-- With the point P defined in terms of the camera coordinate system we can
-- compute P':
--
-- P'.y = P_camera.y / -P_camera.z
-- P'.x = P_camera.x / -P_camera.z
--
-- The (weak) perspective projection operation divides points in camera space
-- with the inverse of the point's z coordinate.

-- | Screen space to raster space
-- The origin of raster space is the top left. X grows left and Y grows
-- downwards. Each point is a pixel on the screen.
--
-- First, convert P' into the range [0, 1]
--
-- P'_norm_x = (P'.x + width / 2) / 2
-- P'_norm_y = (P'.y + height / 2) / 2
--
-- Points are now in Normalized Device Coordinate space (NDC space).
-- The origin of NDC space is defined in the bottom left of the canvas.
--
-- Finally, coordinates are converted into raster space:
--
-- P'_raster_x = floor(P'_norm_x * width)
-- P'_raster_y = floor((1 - P'_norm_y) * height)

cube :: [Line]
cube =
  [ (a, b)
  , (b, c)
  , (c, d)
  , (d, a)

  , (e, f)
  , (f, g)
  , (g, h)
  , (h, e)

  , (a, e)
  , (b, f)
  , (c, g)
  , (d, h)
  ]
  where
    a = vector [0  , 0  , -5 , 1]
    b = vector [10 , 0  , -5 , 1]
    c = vector [10 , 10 , -5 , 1]
    d = vector [0  , 10 , -5 , 1]
    e = vector [0  , 0  ,  5 , 1]
    f = vector [10 , 0  ,  5 , 1]
    g = vector [10 , 10 ,  5 , 1]
    h = vector [0  , 10 ,  5 , 1]


drawLine :: Buffer -> [Matrix Double] -> Line -> IO [Point]
drawLine buf transforms (p1, p2) = bresenhamn buf False r1 r2
  where
    r1 = worldToRaster transforms p1
    r2 = worldToRaster transforms p2

sizeF = fromIntegral size

worldToRaster :: [Matrix Double] -> Vector Double -> Point
worldToRaster transforms point = (rasterX, rasterY)
  where
    [camX, camY, camZ, _] = toList $ foldl (<#) point transforms

    -- weak perspective transform
    screenX = camX / (- camZ)
    screenY = camY / (- camZ)

    ndcX = (screenX + 2.0 * 0.5) / 2.0
    ndcY = (screenY + 2.0 * 0.5) / 2.0

    rasterX = floor (ndcX * sizeF)
    rasterY = floor ((1 - ndcY) * sizeF)

data Color = Color
  { _red   :: Word8
  , _green :: Word8
  , _blue  :: Word8
  , _alpha :: Word8
  }

red = Color 3 0 0 0
green = Color 0 3 0 0
black = Color 0 0 0 0

application :: Buffer -> WS.ServerApp
application buf pending = do
  conn <- WS.acceptRequest pending
  iterateM_ frame (conn, 0, [])
  where
    transforms d = [yRot d, worldToCamera]
    frame (conn, d, old) = do
      putStrLn (show d)
      mapM (\p -> (drawPoint buf p black)) old
      points <- mapM (drawLine buf $ transforms d) cube
      msg <- arrayToBS buf
      WS.sendBinaryData conn msg
      threadDelay 16666
      return (conn, d + 0.01, concat points)

runSocket :: IO ()
runSocket = do
  putStrLn "running"
  buf <- newBuffer
  WS.runServer "127.0.0.1" 9000 (application buf)
