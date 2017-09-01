module Window where

import Control.Exception (throwIO)
import System.Exit

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

import Util.Common
import Movement

winWidth :: Int
winWidth = 800

winHeight :: Int
winHeight = 600

winTitle :: String
winTitle = "HELL YEAH"

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

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

setupWindow :: GLFW.Window -> MovementRefs -> IO ()
setupWindow window (MovementRefs mouseRef keyRef) = do
  (x, y) <- GLFW.getFramebufferSize window
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  GLFW.setKeyCallback window (Just $ keyCallback keyRef)
  GLFW.setCursorPosCallback window (Just $ mouseCallback mouseRef)
  GLFW.makeContextCurrent (Just window)
  glEnable GL_DEPTH_TEST
  glViewport 0 0 (fromIntegral x) (fromIntegral y)
