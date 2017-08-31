module Graphics.Render where

import Control.Monad.Loops (iterateM_)
import Control.Monad (unless)
import Data.IORef (readIORef)
import Data.Bits ((.|.))
import Foreign hiding (rotate)
import Foreign.C.String
import System.Exit

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear hiding (rotate)

import Entity
import Movement
import Util.Model

terminate :: IO ()
terminate = GLFW.terminate >> exitSuccess

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

applyUniform :: M44 GLfloat -> GLint -> (Ptr (V4 (V4 GLfloat))) -> IO ()
applyUniform mat loc mem = do
  poke mem (transpose mat)
  glUniformMatrix4fv loc 1 GL_FALSE (castPtr mem)

applyUniformVec :: V3 GLfloat -> GLint -> Ptr (V3 GLfloat) -> IO ()
applyUniformVec vec loc mem = do
  poke mem vec
  glUniform3fv loc 1 (castPtr mem)

render :: RenderData -> Entity -> IO ()
render rd (Entity (VaoModel vaoID numVertices) pos rot scale) = do
  let modelM = mkTransformation rot pos
  applyUniform modelM (_modelLoc rd) (_modelP rd)
  glBindVertexArray vaoID
  glEnableVertexAttribArray 0
  glEnableVertexAttribArray 1
  glDrawElements GL_TRIANGLES numVertices GL_UNSIGNED_INT nullPtr
  glDisableVertexAttribArray 0
  glDisableVertexAttribArray 1
  glBindVertexArray 0

displayLoop :: GLFW.Window -> RenderData -> [Entity] -> IO ()
displayLoop window renderData entities = iterateM_ loop (0.0, initCamera)
  where
    loop (lastTime, oldCamera) = do
      shouldContinue <- not <$> GLFW.windowShouldClose window
      unless shouldContinue terminate

      GLFW.pollEvents
      glClearColor 0.2 0.3 0.3 1.0
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

      t <- (maybe 0 realToFrac <$> GLFW.getTime) :: IO GLfloat
      keys <- readIORef (_keysRef renderData)
      mouse <- readIORef (_mouseRef renderData)
      let camera = updateCamera keys mouse oldCamera lastTime t
      let viewM = toViewMatrix camera

      -- assign uniforms
      applyUniform viewM (_viewLoc renderData) (_viewP renderData)

      let es  = transformEntities t entities

      mapM_ (render renderData) es
      GLFW.swapBuffers window
      return (t, camera)
