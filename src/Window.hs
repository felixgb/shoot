module Window where

import Control.Exception (throwIO, catch)
import qualified Data.Set as Set
import Data.IORef
import Foreign hiding (rotate)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear hiding (rotate)

import Graphics.Render
import Graphics.Shaders
import Util.Common
import Util.Model
import Parser.ObjectParser (parseObjectFromFile)
import Movement
import Entity

winWidth :: Int
winWidth = 800

winHeight :: Int
winHeight = 600

winTitle :: String
winTitle = "HELL YEAH"

initWindow :: IO GLFW.Window
initWindow = do
  _ <- GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'DepthBits 16)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  mon <- GLFW.getPrimaryMonitor
  maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
  case maybeWindow of
    Just window -> return window
    Nothing     -> throwIO WindowCreationError

setupWindow :: GLFW.Window -> KeysRef -> MouseRef -> IO ()
setupWindow window keyRef mouseRef = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  (x, y) <- GLFW.getFramebufferSize window
  GLFW.setKeyCallback window (Just $ keyCallback keyRef)
  GLFW.setCursorPosCallback window (Just $ mouseCallback mouseRef)
  GLFW.makeContextCurrent (Just window)
  glEnable GL_DEPTH_TEST
  glViewport 0 0 (fromIntegral x) (fromIntegral y)


setupData :: KeysRef -> MouseRef -> IO ([Entity], RenderData)
setupData keyRef mouseRef = do
  shaderProgram <- initShaders
    [ (GL_VERTEX_SHADER, "src/glsl/vertex.shader")
    , (GL_FRAGMENT_SHADER, "src/glsl/fragment.shader")
    ]
  -- cube       <- parseObjectFromFile "resources/cube.obj"
  teapot     <- parseObjectFromFile "resources/teapot.obj"
  -- dragon     <- parseObjectFromFile "resources/dragon.obj"
  vao1       <- loadToVao teapot
  vao2       <- loadToVao teapot
  model      <- getUniformLocation "model" shaderProgram
  view       <- getUniformLocation "view" shaderProgram
  projection <- getUniformLocation "projection" shaderProgram
  lightPos   <- getUniformLocation "lightPos" shaderProgram
  lightColor <- getUniformLocation "lightColor" shaderProgram
  projP      <- malloc
  modelP     <- malloc
  viewP      <- malloc
  lightPosP     <- malloc
  lightColorP   <- malloc
  let e1     =  Entity vao1 (V3 1 0 0) (axisAngle (V3 0 0 0) 0) 1
  let e2     =  Entity vao2 (V3 0 0 (-5)) (axisAngle (V3 0 0 0) 0) 1

  let screenWidth  = fromIntegral winWidth :: GLfloat
  let screenHeight = fromIntegral winHeight :: GLfloat
  let projM        = perspective 45 (screenWidth / screenHeight) 0.1 100.0
  applyUniform projM projection projP
  applyUniformVec (V3 0 3 0) lightPos lightPosP
  applyUniformVec (V3 1 1 1) lightColor lightColorP

  return $ ([e1, e2], RenderData
    { _modelLoc = model
    , _modelP   = modelP
    , _viewLoc  = view
    , _viewP    = viewP
    , _keysRef  = keyRef
    , _mouseRef  = mouseRef
    })

go :: IO ()
go = catch body handler
  where
    handler :: ShootError -> IO ()
    handler err = print err >> GLFW.terminate
    body = do
      keyRef   <- newIORef Set.empty
      mouseRef <- newIORef initMouse
      window   <- initWindow
      setupWindow window keyRef mouseRef
      (es, renderData ) <- setupData keyRef mouseRef
      displayLoop window renderData es
