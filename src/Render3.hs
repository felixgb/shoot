module Render3 where

import Control.Monad.Reader
import Control.Monad (when, unless, forever)
import Control.Exception
import qualified Data.Map as Map
import Foreign
import Foreign.C.String (CString, newCAStringLen, newCString)
import System.Exit

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

type ModelData = Map.Map GLuint (V3 GLfloat)

data RenderData = RenderData
  { _models          :: [U.VaoModel]
  , _shaderProgram   :: GLuint
  , _model           :: CString
  , _modelP          :: Ptr (V4 (V4 GLfloat))
  , _view            :: CString
  , _viewP           :: Ptr (V4 (V4 GLfloat))
  , _projection      :: CString
  , _projP           :: Ptr (V4 (V4 GLfloat))
  , _projM           :: M44 GLfloat
  , _positions       :: ModelData
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

verticies2 = [
    -- positions
    0.2  , 0.2  , 0.0 , -- Top Right
    0.2  , -0.2 , 0.0 , -- Bottom Right
    -0.2 , -0.2 , 0.0 , -- Bottom Left
    -0.2 , 0.2  , 0.0   -- Top Left
  ] :: [GLfloat]

indices = [  -- Note that we start from 0!
  0, 1, 3, -- First Triangle
  1, 2, 3  -- Second Triangle
  ] :: [GLuint]

setupWindow :: GLFW.Window -> IO ()
setupWindow window = do
  (x, y) <- GLFW.getFramebufferSize window
  GLFW.setKeyCallback window (Just keyCallback)
  GLFW.makeContextCurrent (Just window)
  glViewport 0 0 (fromIntegral x) (fromIntegral y)

setupData :: IO RenderData
setupData = do
  shaderProgram <- U.initShaders
    [ (GL_VERTEX_SHADER, "src/glsl/vertex.shader")
    , (GL_FRAGMENT_SHADER, "src/glsl/fragment.shader")
    ]
  vao1       <- U.loadToVao $ U.Object verticies colors indices
  vao2       <- U.loadToVao $ U.Object verticies2 colors indices
  model      <- newCString "model"
  modelP     <- malloc
  view       <- newCString "view"
  viewP      <- malloc
  projection <- newCString "projection"
  projP      <- malloc
  let screenWidth = fromIntegral winWidth :: GLfloat
  let screenHeight = fromIntegral winHeight :: GLfloat
  let projM  = perspective 45 (screenWidth / screenHeight) 0.1 100.0
  let positions = Map.fromList $ map (\((U.VaoModel id _), p) -> (id, p)) $ zip [vao1, vao2] [V3 0 0 0, V3 1 0 0]
  return $ RenderData
    { _models          = [vao1, vao2]
    , _shaderProgram   = shaderProgram
    , _model           = model
    , _modelP          = modelP
    , _view            = view
    , _viewP           = viewP
    , _projection      = projection
    , _projP           = projP
    , _projM           = projM
    , _positions       = positions
    }

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

displayLoop :: GLFW.Window -> RenderData -> IO ()
displayLoop window renderData = forever $ do
  shouldContinue <- not <$> GLFW.windowShouldClose window
  unless shouldContinue terminate

  GLFW.pollEvents
  timeValue <- maybe 0 realToFrac <$> GLFW.getTime
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

  -- assign uniforms
  let shaderProgram = _shaderProgram renderData
  modelLoc <- glGetUniformLocation shaderProgram (_model renderData)
  viewLoc <- glGetUniformLocation shaderProgram (_view renderData)
  projLoc <- glGetUniformLocation shaderProgram (_projection renderData)

  let viewM = mkTransformation (axisAngle (V3 (0 :: GLfloat) 0 1) 0) (V3 0 0 (-3))
  let projM = _projM renderData
  poke (_viewP renderData) (transpose viewM)
  poke (_projP renderData) (transpose projM)
  glUniformMatrix4fv viewLoc 1 GL_FALSE (castPtr $ _viewP renderData)
  glUniformMatrix4fv projLoc 1 GL_FALSE (castPtr $ _projP renderData)

  forM_ (_models renderData) $ \(U.VaoModel id numVertices) -> do
    let position = (_positions renderData) Map.! id
    let modelM = mkTransformation (axisAngle (V3 (0.5 :: GLfloat) 0 0) timeValue) position
    poke (_modelP renderData) (transpose modelM)
    glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr $ _modelP renderData)
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
