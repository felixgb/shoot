module Render2 where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import qualified Data.Vector.Storable as V

import Data.IORef

go :: IO ()
go = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  initialWindowSize $= Size 100 100
  createWindow "FUN STUFF!!!"
  xDelta <- newIORef 0.0
  yDelta <- newIORef 0.0
  depthFunc       $= Just Less
  displayCallback $= (display xDelta yDelta)
  idleCallback $= Just idle
  reshapeCallback $= Just reshape
  passiveMotionCallback   $= Just (mouse xDelta yDelta)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: IORef GLfloat -> IORef GLfloat -> DisplayCallback
display xDelta yDelta = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  x <- get xDelta
  y <- get yDelta
  perspective 90 1 0.1 10
  translate $ Vector3 (0 :: GLfloat) (0 :: GLfloat) (-1 :: GLfloat)
  rotate x $ Vector3 1 0 0
  rotate y $ Vector3 0 1 0
  preservingMatrix draw

  swapBuffers

draw = do
  color $ Color3 (0 :: GLfloat) (255 :: GLfloat) (0 :: GLfloat)
  vertexAttribArray (AttribLocation 0) $= Enabled
  V.unsafeWith cube2 $ \ptr -> do
    vertexAttribPointer (AttribLocation 0) $=
      (ToFloat, VertexArrayDescriptor 3 Float 0 ptr)
  drawArrays Triangles 0 (12 * 3)
  vertexAttribArray (AttribLocation 0) $= Disabled
  -- cubeFrame 0.11

cube2 :: V.Vector Float
cube2 = V.fromList [
    -0.1,-0.1,-0.1,
    -0.1,-0.1, 0.1,
    -0.1, 0.1, 0.1,
    0.1, 0.1,-0.1,
    -0.1,-0.1,-0.1,
    -0.1, 0.1,-0.1,
    0.1,-0.1, 0.1,
    -0.1,-0.1,-0.1,
    0.1,-0.1,-0.1,
    0.1, 0.1,-0.1,
    0.1,-0.1,-0.1,
    -0.1,-0.1,-0.1,
    -0.1,-0.1,-0.1,
    -0.1, 0.1, 0.1,
    -0.1, 0.1,-0.1,
    0.1,-0.1, 0.1,
    -0.1,-0.1, 0.1,
    -0.1,-0.1,-0.1,
    -0.1, 0.1, 0.1,
    -0.1,-0.1, 0.1,
    0.1,-0.1, 0.1,
    0.1, 0.1, 0.1,
    0.1,-0.1,-0.1,
    0.1, 0.1,-0.1,
    0.1,-0.1,-0.1,
    0.1, 0.1, 0.1,
    0.1,-0.1, 0.1,
    0.1, 0.1, 0.1,
    0.1, 0.1,-0.1,
    -0.1, 0.1,-0.1,
    0.1, 0.1, 0.1,
    -0.1, 0.1,-0.1,
    -0.1, 0.1, 0.1,
    0.1, 0.1, 0.1,
    -0.1, 0.1, 0.1,
    0.1,-0.1, 0.1
    ]

mouse :: IORef GLfloat -> IORef GLfloat -> MotionCallback
mouse xDelta yDelta (Position x y) = do
  Size width height <- get windowSize
  putStrLn $ show (x, width)
  let xDiff = ((conv x) / (conv width)) - 0.5
  let yDiff = ((conv y) / (conv height)) - 0.5
  putStrLn $ show (xDiff, yDiff)
  yDelta $~! (+ xDiff)
  xDelta $~! (+ yDiff)
  where conv n = (realToFrac $ fromIntegral n) :: GLfloat

idle :: IdleCallback
idle = do
  -- angle $~! (+ 0.5)
  postRedisplay Nothing

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]
