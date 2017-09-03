module Window where

import Control.Monad (unless)
import Control.Exception (throwIO)
import System.Exit

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

import Util.Common
import Movement

winWidth :: Int
winWidth = 1920

winHeight :: Int
winHeight = 1080

winTitle :: String
winTitle = "HELL YEAH"

shouldTerminate :: GLFW.Window -> IO ()
shouldTerminate window = do
  shouldContinue <- not <$> GLFW.windowShouldClose window
  unless shouldContinue (GLFW.terminate >> exitSuccess)

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

setupWindow :: GLFW.Window -> MovementRefs -> ClickRef -> IO ()
setupWindow window (MovementRefs mouseRef keyRef) clickRef = do
  (x, y) <- GLFW.getFramebufferSize window
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  GLFW.setKeyCallback window (Just $ keyCallback keyRef)
  GLFW.setCursorPosCallback window (Just $ mouseCallback mouseRef)
  GLFW.setMouseButtonCallback window (Just $ clickCallback clickRef)
  GLFW.makeContextCurrent (Just window)
  glEnable GL_DEPTH_TEST
  glViewport 0 0 (fromIntegral x) (fromIntegral y)
