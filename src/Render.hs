{-# LANGUAGE OverloadedStrings #-}

module Render where

import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.GL.Core31 (glUniformMatrix3fv, glUniformMatrix4fv)
import qualified Graphics.UI.GLFW as GLFW
import Data.Fixed
import Numeric.LinearAlgebra hiding (loadMatrix)
import Graphics.Rendering.OpenGL.GLU.Matrix

import qualified Util.Shaders as U
import qualified Util.GLFW as U

go :: IO ()
go = do
  win <- U.initialize "Multicolor triangle"
  p <- initResources
  U.mainLoop (draw p win) win
  U.cleanup win

initResources :: IO GL.Program
initResources = do
  vSh <- initShader "src/glsl/vertexShader.glsl" GL.VertexShader
  fSh <- initShader "src/glsl/fragmentShader.glsl" GL.FragmentShader

  program <- GL.createProgram
  GL.attachShader program vSh
  GL.attachShader program fSh
  GL.attribLocation program "coord" $= GL.AttribLocation 0
  GL.attribLocation program "color" $= GL.AttribLocation 1
  GL.linkProgram program
  pIsOk <- GL.get $ GL.linkStatus program
  GL.validateProgram program
  status <- GL.get $ GL.validateStatus program
  unless (pIsOk && status) $ do
    pLog <- GL.get $ GL.programInfoLog program
    putStrLn pLog
    exitFailure

  GL.currentProgram $= Just program
  GL.depthFunc $= Just GL.Less

  U.printError
  return program
  where
    initShader path shaderType = do
      sh <- GL.createShader shaderType
      source <- BS.readFile path
      GL.shaderSourceBS sh $= source
      GL.compileShader sh
      isOk <- GL.get $ GL.compileStatus sh
      unless isOk $ do
        sLog <- GL.get $ GL.shaderInfoLog sh
        putStrLn $ "Log: " ++ sLog
        exitFailure
      return sh

draw :: GL.Program -> GLFW.Window -> IO ()
draw p w = do
  (width, height) <- GLFW.getFramebufferSize w
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  let ratio = fromIntegral width / fromIntegral height

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.currentProgram $= Just p
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  V.unsafeWith cube $ \ptr -> do
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)

  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

  V.unsafeWith cubeColor $ \ptr -> do
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)

  Just t <- GLFW.getTime

  perspective (90.0 :: GL.GLdouble) ratio near far

  GL.drawArrays GL.Triangles 0 (12 * 3)
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled

loadMatrix :: GL.Program -> String -> V.Vector GL.GLfloat -> IO ()
loadMatrix program name vec = do
  GL.UniformLocation pLoc <- GL.get $ GL.uniformLocation program name
  V.unsafeWith vec $ \ptr -> do
    glUniformMatrix4fv pLoc 1 1 ptr

translate :: Double -> Double -> Double -> V.Vector GL.GLfloat
translate x' y' z' = V.fromList
  [ 1 , 0 , 0 , x
  , 0 , 1 , 0 , y
  , 0 , 0 , 1 , z
  , 0 , 0 , 0 , 1
  ]
  where
    x = realToFrac x'
    y = realToFrac y'
    z = realToFrac z'

near :: GL.GLdouble
near = 0.1

far :: GL.GLdouble
far = 10

rotateX :: Double -> Matrix GL.GLfloat
rotateX t' = (4 >< 4)
  [  1,     0,      0, 0
  ,  0, cos t, -sin t, 0
  ,  0, sin t, cos t,  0
  ,  0, 0,         0,  1
  ]
  where t = realToFrac t'

rotateZ :: Double -> Matrix GL.GLfloat
rotateZ t' = (4 >< 4)
  [  cos t, -sin t, 0, 0
  ,  sin t,  cos t, 0, 0
  ,      0,      0, 1, 0
  ,      0,      0, 0, 1
  ]
  where t = realToFrac t'

rotateY :: Double -> Matrix GL.GLfloat
rotateY t' = (4 >< 4)
  [   cos t, 0, sin t, 0
  ,       0, 1,     0, 0
  ,  -sin t, 0, cos t, 0
  ,       0, 0,     0, 1
  ]
  where t = realToFrac t'

matrix2vector :: Matrix GL.GLfloat -> V.Vector GL.GLfloat
matrix2vector = V.fromList . concat . toLists

colors :: V.Vector Float
colors = V.fromList
  [ 1, 0, 0
  , 0, 1, 0
  , 0, 0, 1
  ]

vertices :: V.Vector Float
vertices = V.fromList
  [ -0.6, -0.4, 0
  ,  0.6, -0.4, 0
  ,    0,  0.6, 0
  ]

cube :: V.Vector Float
cube = V.fromList [
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

cubeColor :: V.Vector Float
cubeColor = V.fromList [
    0.583,  0.771,  0.014,
    0.609,  0.115,  0.436,
    0.327,  0.483,  0.844,
    0.822,  0.569,  0.201,
    0.435,  0.602,  0.223,
    0.310,  0.747,  0.185,
    0.597,  0.770,  0.761,
    0.559,  0.436,  0.730,
    0.359,  0.583,  0.152,
    0.483,  0.596,  0.789,
    0.559,  0.861,  0.639,
    0.195,  0.548,  0.859,
    0.014,  0.184,  0.576,
    0.771,  0.328,  0.970,
    0.406,  0.615,  0.116,
    0.676,  0.977,  0.133,
    0.971,  0.572,  0.833,
    0.140,  0.616,  0.489,
    0.997,  0.513,  0.064,
    0.945,  0.719,  0.592,
    0.543,  0.021,  0.978,
    0.279,  0.317,  0.505,
    0.167,  0.620,  0.077,
    0.347,  0.857,  0.137,
    0.055,  0.953,  0.042,
    0.714,  0.505,  0.345,
    0.783,  0.290,  0.734,
    0.722,  0.645,  0.174,
    0.302,  0.455,  0.848,
    0.225,  0.587,  0.040,
    0.517,  0.713,  0.338,
    0.053,  0.959,  0.120,
    0.393,  0.621,  0.362,
    0.673,  0.211,  0.457,
    0.820,  0.883,  0.371,
    0.982,  0.099,  0.879
    ]
