module Render3 where

import Control.Monad.Reader
import Control.Monad (when, unless, forever)
import Control.Exception
import System.Exit
import Foreign
import Foreign.C.String (CString, newCAStringLen, newCString)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear

import qualified Util.Common as U
import qualified Util.Shaders as U
import qualified Util.Model as U

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

data RenderData = RenderData
  { _models          :: [U.VaoModel]
  , _shaderProgram   :: GLuint
  , _transformString :: CString
  , _transP          :: Ptr (V4 (V4 GLfloat))
  }

colors = [
    1.0 , 0.0 , 0.0 , -- Top Right
    0.0 , 0.0 , 1.0 , -- Bottom Right
    0.0 , 1.0 , 0.0 , -- Bottom Left
    1.0 , 1.0 , 0.0   -- Top Left
  ] :: [GLfloat]

verticies = [
    -- positions
    0.5  , 0.5  , 0.0 , -- Top Right
    0.5  , -0.5 , 0.0 , -- Bottom Right
    -0.5 , -0.5 , 0.0 , -- Bottom Left
    -0.5 , 0.5  , 0.0   -- Top Left
  ] :: [GLfloat]

indices = [  -- Note that we start from 0!
  0, 1, 3, -- First Triangle
  1, 2, 3  -- Second Triangle
  ] :: [GLuint]

setupWindow :: GLFW.Window -> IO ()
setupWindow window = do
  GLFW.setKeyCallback window (Just keyCallback)
  GLFW.makeContextCurrent (Just window)
  (x, y) <- GLFW.getFramebufferSize window
  glViewport 0 0 (fromIntegral x) (fromIntegral y)


setupData :: IO RenderData
setupData = do
  shaderProgram <- U.initShaders
    [ (GL_VERTEX_SHADER, "src/glsl/vertex.shader")
    , (GL_FRAGMENT_SHADER, "src/glsl/fragment.shader")
    ]
  model     <- U.loadToVao $ U.Object verticies colors indices
  transform <- newCString "transform"
  transP    <- malloc
  return $ RenderData [model] shaderProgram transform transP

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

displayLoop :: GLFW.Window -> RenderData -> IO ()
displayLoop window renderData = forever $ do
  shouldContinue <- not <$> GLFW.windowShouldClose window
  unless shouldContinue terminate

  GLFW.pollEvents
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

  timeValue <- maybe 0 realToFrac <$> GLFW.getTime
  let rotQ = axisAngle (V3 (0 :: GLfloat) 0 1) timeValue
  let rotM33 = fromQuaternion rotQ
  let rotM33' = rotM33 !!* 0.5
  let transformationMatrix = mkTransformationMat rotM33' (V3 0.0 (-0.0) 0)
  poke (_transP renderData) (transpose transformationMatrix)
  transformLoc <- glGetUniformLocation (_shaderProgram renderData) (_transformString renderData)
  glUniformMatrix4fv transformLoc 1 GL_FALSE (castPtr (_transP renderData))

  glUseProgram (_shaderProgram renderData)

  forM_ (_models renderData) $ \(U.VaoModel id numVertices) -> do
    glBindVertexArray id
    glDrawElements GL_TRIANGLES numVertices GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

  GLFW.swapBuffers window

go :: IO ()
go = catch body handler
  where
    handler :: U.ShootError -> IO ()
    handler ex = print ex >> GLFW.terminate
    body = do
      window <- initWindow
      setupWindow window
      renderData <- setupData
      displayLoop window renderData
