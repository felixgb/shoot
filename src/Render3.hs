module Render3 where

import Control.Monad (when, unless, forever)
import Control.Exception
import System.Exit
import Foreign
import Foreign.C.String (newCAStringLen, newCString)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear

import qualified Util.Common as U
import qualified Util.Shaders as U

winWidth = 800
winHeight = 600

winTitle = "HELL YEAH"

keyCallback :: GLFW.KeyCallback
keyCallback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

initWindow :: IO GLFW.Window
initWindow = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
  case maybeWindow of
    Just window -> return window
    Nothing     -> throwIO U.WindowCreationError

setCallbacks :: GLFW.Window -> IO (GLuint, GLuint)
setCallbacks window = do
  putStrLn "called"
  GLFW.setKeyCallback window (Just keyCallback)
  GLFW.makeContextCurrent (Just window)
  (x, y) <- GLFW.getFramebufferSize window
  glViewport 0 0 (fromIntegral x) (fromIntegral y)

  shaderProgram <- U.initShaders
    [ (GL_VERTEX_SHADER, "src/glsl/vertex.shader")
    , (GL_FRAGMENT_SHADER, "src/glsl/fragment.shader")
    ]

  let verticies = [
        0.5,  0.5, 0.0,  -- Top Right
        0.5, -0.5, 0.0,  -- Bottom Right
        -0.5, -0.5, 0.0, -- Bottom Left
        -0.5,  0.5, 0.0  -- Top Left
          ] :: [GLfloat]
  let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
  verticesP <- newArray verticies

  let indices = [  -- Note that we start from 0!
          0, 1, 3, -- First Triangle
          1, 2, 3  -- Second Triangle
          ] :: [GLuint]
  let indicesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length indices)
  indicesP <- newArray indices

  -- setup vertex array object
  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao

  -- setup vertex buffer object and send it data
  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

  eboP <- malloc
  glGenBuffers 1 eboP
  ebo <- peek eboP

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

  let floatSize = (fromIntegral $ sizeOf (0.0 :: GLfloat)) :: GLsizei
  -- position attribute
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (3 * floatSize) nullPtr
  glEnableVertexAttribArray 0

  -- color attribute, where do these come from
  let threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3 * floatSize)
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (2 * floatSize) threeFloatOffset
  glEnableVertexAttribArray 1

  glBindVertexArray 0
  return (vao, shaderProgram)

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

displayLoop :: GLFW.Window -> GLuint -> GLuint -> IO ()
displayLoop window vao shaderProgram = forever $ do
  shouldContinue <- not <$> GLFW.windowShouldClose window
  unless shouldContinue terminate

  GLFW.pollEvents
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

  transform <- newCString "transform"

  transP <- malloc
  timeValue <- maybe 0 realToFrac <$> GLFW.getTime
  let rotQ = axisAngle (V3 (0 :: GLfloat) 0 1) timeValue
  let rotM33 = fromQuaternion rotQ
  let rotM33' = rotM33 !!* 0.5
  let transformationMatrix = mkTransformationMat rotM33' (V3 0.5 (-0.5) 0)
  poke transP (transpose transformationMatrix)
  transformLoc <- glGetUniformLocation shaderProgram transform
  glUniformMatrix4fv transformLoc 1 GL_FALSE (castPtr transP)

  glUseProgram shaderProgram
  glBindVertexArray vao
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0

  GLFW.swapBuffers window

go :: IO ()
go = catch body handler
  where
    handler :: U.ShootError -> IO ()
    handler ex = print ex >> GLFW.terminate
    body = do
      window <- initWindow
      (vao, shaderProgram) <- setCallbacks window
      displayLoop window vao shaderProgram
