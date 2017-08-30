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
import Linear

import qualified Util.Common as U
import qualified Util.Shaders as U
import qualified Util.Model as U
import qualified Entity as E
import ObjectParser (parseObjectFromFile)

winWidth = 800
winHeight = 600

winTitle = "HELL YEAH"

type KeysRef = IORef (Set GLFW.Key)

keyCallback :: KeysRef -> GLFW.KeyCallback
keyCallback ref window key scanCode keyState modKeys = do
    case keyState of
      GLFW.KeyState'Pressed -> modifyIORef ref (Set.insert key)
      GLFW.KeyState'Released -> modifyIORef ref (Set.delete key)
      _ -> return ()
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

setupWindow :: GLFW.Window -> KeysRef -> IO ()
setupWindow window ref = do
  (x, y) <- GLFW.getFramebufferSize window
  GLFW.setKeyCallback window (Just $ keyCallback ref)
  GLFW.makeContextCurrent (Just window)
  glViewport 0 0 (fromIntegral x) (fromIntegral y)

data Camera = Camera
  { _pos   :: V3 GLfloat
  , _front :: V3 GLfloat
  , _up    :: V3 GLfloat
  }

updateCamera :: GLfloat -> Camera -> Set GLFW.Key -> Camera
updateCamera speed = Set.foldr modCam
  where
    modCam key cam@(Camera pos front up) = case key of
      GLFW.Key'W -> cam { _pos = pos ^+^ (speed *^ front) }
      GLFW.Key'S -> cam { _pos = pos ^-^ (speed *^ front) }
      GLFW.Key'A -> cam { _pos = pos ^-^ (speed *^ (normalize (cross front up))) }
      GLFW.Key'D -> cam { _pos = pos ^+^ (speed *^ (normalize (cross front up))) }
      _ -> cam

toViewMatrix :: Camera -> M44 GLfloat
toViewMatrix (Camera pos front up) = lookAt pos (pos ^+^ front) up

data RenderData = RenderData
  { _model           :: GLint
  , _modelP          :: Ptr (V4 (V4 GLfloat))
  , _view            :: GLint
  , _viewP           :: Ptr (V4 (V4 GLfloat))
  , _keysRef         :: KeysRef
  }

getUniformLocation :: String -> GLuint -> IO GLint
getUniformLocation name prog = withCString name (glGetUniformLocation prog)

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

setupData :: KeysRef -> IO ([E.Entity], RenderData)
setupData ref = do
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
    , _keysRef         = ref
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

initCamera :: Camera
initCamera = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0)

displayLoop :: GLFW.Window -> RenderData -> [E.Entity] -> IO ()
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
      let camera = updateCamera cameraSpeed oldCamera keysDown
      let viewM = toViewMatrix camera
      -- assign uniforms
      applyUniform viewM (_view renderData) (_viewP renderData)

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
      ref <- newIORef Set.empty
      window <- initWindow
      setupWindow window ref
      (es, renderData ) <- setupData ref
      displayLoop window renderData es
