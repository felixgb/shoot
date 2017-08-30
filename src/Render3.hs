module Render3 where

import Control.Monad.Loops (iterateM_)
import Control.Monad.Reader
import Control.Monad (when, unless, forever)
import Control.Exception
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign hiding (rotate)
import Foreign.C.String
import System.Exit

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear hiding (rotate)

import qualified Util.Common as U
import qualified Util.Shaders as U
import qualified Util.Model as U
import ObjectParser (parseObjectFromFile)
import Entity
import Movement

winWidth = 2560
winHeight = 1440

winTitle = "HELL YEAH"

initWindow :: IO GLFW.Window
initWindow = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  mon <- GLFW.getPrimaryMonitor
  maybeWindow <- GLFW.createWindow winWidth winHeight winTitle mon Nothing
  case maybeWindow of
    Just window -> return window
    Nothing     -> throwIO U.WindowCreationError

setupWindow :: GLFW.Window -> KeysRef -> MouseRef -> IO ()
setupWindow window keyRef mouseRef = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  (x, y) <- GLFW.getFramebufferSize window
  GLFW.setKeyCallback window (Just $ keyCallback keyRef)
  GLFW.setCursorPosCallback window (Just $ mouseCallback mouseRef)
  GLFW.makeContextCurrent (Just window)
  glViewport 0 0 (fromIntegral x) (fromIntegral y)

data RenderData = RenderData
  { _modelLoc :: GLint
  , _modelP   :: Ptr (V4 (V4 GLfloat))
  , _viewLoc  :: GLint
  , _viewP    :: Ptr (V4 (V4 GLfloat))
  , _keysRef  :: KeysRef
  , _mouseRef :: MouseRef
  }

getUniformLocation :: String -> GLuint -> IO GLint
getUniformLocation name prog = withCString name (glGetUniformLocation prog)

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

setupData :: KeysRef -> MouseRef -> IO ([Entity], RenderData)
setupData keyRef mouseRef = do
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
  let e1     =  Entity vao1 (V3 1 0 0) (axisAngle (V3 0 0 0) 0) 1
  let e2     =  Entity vao2 (V3 0 0 (-5)) (axisAngle (V3 0 0 0) 0) 1

  let screenWidth  = fromIntegral winWidth :: GLfloat
  let screenHeight = fromIntegral winHeight :: GLfloat
  let projM        = perspective 45 (screenWidth / screenHeight) 0.1 100.0
  applyUniform projM projection projP

  return $ ([e1, e2], RenderData
    { _modelLoc = model
    , _modelP   = modelP
    , _viewLoc  = view
    , _viewP    = viewP
    , _keysRef  = keyRef
    , _mouseRef  = mouseRef
    })

transformEntities :: GLfloat -> [Entity] -> [Entity]
transformEntities delta [e1, e2] = [r1, r2]
  where
    r1 = rotate (delta * 10) (V3 0 1 0) e1
    r2 = rotate delta (V3 1 0 0) e2

applyUniform :: M44 GLfloat -> GLint -> (Ptr (V4 (V4 GLfloat))) -> IO ()
applyUniform mat loc mem = do
  poke mem (transpose mat)
  glUniformMatrix4fv loc 1 GL_FALSE (castPtr mem)

render :: RenderData -> Entity -> IO ()
render rd (Entity (U.VaoModel id numVertices) pos rot scale) = do
  let modelM = mkTransformation rot pos
  applyUniform modelM (_modelLoc rd) (_modelP rd)
  glBindVertexArray id
  glDrawElements GL_TRIANGLES numVertices GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0

displayLoop :: GLFW.Window -> RenderData -> [Entity] -> IO ()
displayLoop window renderData entities = iterateM_ loop (0.0, initCamera)
  where
    loop (lastTime, oldCamera) = do
      shouldContinue <- not <$> GLFW.windowShouldClose window
      unless shouldContinue terminate

      GLFW.pollEvents
      glClearColor 0.2 0.3 0.3 1.0
      glClear GL_COLOR_BUFFER_BIT

      t <- (maybe 0 realToFrac <$> GLFW.getTime) :: IO GLfloat
      let deltaTime = t - lastTime
      let cameraSpeed = 5 * deltaTime
      keysDown <- readIORef (_keysRef renderData)
      let cameraTemp = updateCamera cameraSpeed oldCamera keysDown
      mouse <- readIORef (_mouseRef renderData)
      let camera = cameraTemp { _front = _frontVec mouse }
      let viewM = toViewMatrix camera
      -- assign uniforms
      applyUniform viewM (_viewLoc renderData) (_viewP renderData)

      let es  = transformEntities t entities

      mapM (render renderData) es
      GLFW.swapBuffers window
      return (t, camera)

go :: IO ()
go = catch body handler
  where
    handler :: U.ShootError -> IO ()
    handler ex = print ex >> GLFW.terminate
    body = do
      keyRef <- newIORef Set.empty
      mouseRef <- newIORef initMouse
      window <- initWindow
      setupWindow window keyRef mouseRef
      (es, renderData ) <- setupData keyRef mouseRef
      displayLoop window renderData es
