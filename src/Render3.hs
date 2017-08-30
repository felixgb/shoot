module Render3 where

import Control.Monad.Reader
import Control.Monad (when, unless, forever)
import Control.Exception
import qualified Data.Map as Map
import Foreign hiding (rotate)
import Foreign.C.String
import System.Exit

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear

import qualified Util.Common as U
import qualified Util.Shaders as U
import qualified Util.Model as U
import qualified Entity as E
import ObjectParser (parseObjectFromFile)

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
  { _model           :: GLint
  , _modelP          :: Ptr (V4 (V4 GLfloat))
  , _view            :: GLint
  , _viewP           :: Ptr (V4 (V4 GLfloat))
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

getUniformLocation :: String -> GLuint -> IO GLint
getUniformLocation name prog = withCString name (glGetUniformLocation prog)

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

setupData :: IO ([E.Entity], RenderData)
setupData = do
  shaderProgram <- U.initShaders
    [ (GL_VERTEX_SHADER, "src/glsl/vertex.shader")
    , (GL_FRAGMENT_SHADER, "src/glsl/fragment.shader")
    ]
  cube       <- parseObjectFromFile "resources/cube.obj"
  vao1       <- U.loadToVao cube
  vao2       <- U.loadToVao cube
  model      <- getUniformLocation "model" shaderProgram
  view       <- getUniformLocation "view" shaderProgram
  projection <- getUniformLocation "projection" shaderProgram
  projP      <- malloc
  modelP     <- malloc
  viewP      <- malloc
  let e1     =  E.Entity vao1 (V3 1 0 0) (axisAngle (V3 0 0 0) 0) 1
  let e2     =  E.Entity vao2 (V3 0 0 (-5)) (axisAngle (V3 0 0 0) 0) 1

  let screenWidth  = fromIntegral winWidth :: GLfloat
  let screenHeight = fromIntegral winHeight :: GLfloat
  let projM        = perspective 45 (screenWidth / screenHeight) 0.1 100.0
  applyUniform projM projection projP

  return $ ([e1, e2], RenderData
    { _model           = model
    , _modelP          = modelP
    , _view            = view
    , _viewP           = viewP
    })

transformEntities :: GLfloat -> [E.Entity] -> [E.Entity]
transformEntities delta [e1, e2] = [r1, r2]
  where
    r1 = E.rotate (delta * 10) (V3 0 1 0) e1
    r2 = E.rotate delta (V3 1 0 0) e2

applyUniform :: M44 GLfloat -> GLint -> (Ptr (V4 (V4 GLfloat))) -> IO ()
applyUniform mat loc mem = do
  poke mem (transpose mat)
  glUniformMatrix4fv loc 1 GL_FALSE (castPtr mem)

render :: RenderData -> E.Entity -> IO ()
render rd (E.Entity (U.VaoModel id numVertices) pos rot scale) = do
  let modelM = mkTransformation rot pos
  applyUniform modelM (_model rd) (_modelP rd)
  glBindVertexArray id
  glDrawElements GL_TRIANGLES numVertices GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0

displayLoop :: GLFW.Window -> RenderData -> [E.Entity] -> IO ()
displayLoop window renderData entities = forever $ do
  shouldContinue <- not <$> GLFW.windowShouldClose window
  unless shouldContinue terminate

  GLFW.pollEvents
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

  let viewM = mkTransformation (axisAngle (V3 (0 :: GLfloat) 0 1) 0) (V3 0 0 (-3))
  -- assign uniforms
  applyUniform viewM (_view renderData) (_viewP renderData)

  t <- (maybe 0 realToFrac <$> GLFW.getTime) :: IO GLfloat
  let es  = transformEntities t entities

  mapM (render renderData) es
  GLFW.swapBuffers window

go :: IO ()
go = catch body handler
  where
    handler :: U.ShootError -> IO ()
    handler ex = print ex >> GLFW.terminate
    body = do
      window <- initWindow
      setupWindow window
      (es, renderData ) <- setupData
      displayLoop window renderData es
