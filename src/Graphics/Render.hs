module Graphics.Render where

import Control.Monad.Loops (iterateM_)
import Control.Monad (unless, forM_)
import Data.IORef (readIORef)
import Data.Bits ((.|.))
import Foreign hiding (rotate)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear hiding (rotate)

import Entity
import Light
import Movement
import Util.Model
import Graphics.Uniforms

applyLights :: Uniforms -> [Light] -> IO ()
applyLights uniforms lights = forM_ lights $ \(Light pos color) -> do
  applyUniformV3 pos (_ulightPos uniforms)
  applyUniformV3 color (_ulightColor uniforms)

applyProjection :: Uniforms -> GLFW.Window -> IO ()
applyProjection uniforms window = do
  (x, y) <- GLFW.getFramebufferSize window
  let projM = perspective 45 (fromIntegral x / fromIntegral y) 0.1 100.0
  applyUniformM44 projM (_proj uniforms)

applyViewMove :: Uniforms -> MovementRefs -> Camera -> GLfloat -> IO Camera
applyViewMove uniforms moveRef oldCamera lastTime = do
      t     <- (maybe 0 realToFrac <$> GLFW.getTime) :: IO GLfloat
      mouse <- readIORef (_mouseRef moveRef)
      keys  <- readIORef (_keysRef moveRef)
      let camera = updateCamera keys mouse oldCamera lastTime t
      let viewM  = toViewMatrix camera
      applyUniformM44 viewM (_view uniforms)
      return camera

render :: Uniforms -> Entity -> IO ()
render uniforms (Entity (VaoModel vaoID numVertices) pos rot scale) = do
  let modelM = mkTransformation rot pos
  applyUniformM44 modelM (_model uniforms)
  glBindVertexArray vaoID
  -- vertices in attr 0
  glEnableVertexAttribArray 0
  -- vertex normals in attr 1
  glEnableVertexAttribArray 1
  glDrawElements GL_TRIANGLES numVertices GL_UNSIGNED_INT nullPtr
  glDisableVertexAttribArray 0
  glDisableVertexAttribArray 1
  glBindVertexArray 0

initDisplay :: GLFW.Window -> Uniforms -> MovementRefs -> [Entity] -> [Light] -> IO () -> IO ()
initDisplay window uniforms moveRef entities lights exitFunc= do
  applyProjection uniforms window
  applyLights uniforms lights
  iterateM_ loop (0.0, initCamera)
  where
    loop (lastTime, oldCamera) = do
      shouldContinue <- not <$> GLFW.windowShouldClose window
      unless shouldContinue exitFunc
      GLFW.pollEvents
      glClearColor 0.2 0.3 0.3 1.0
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
      camera <- applyViewMove uniforms moveRef oldCamera lastTime
      t <- (maybe 0 realToFrac <$> GLFW.getTime) :: IO GLfloat
      let es  = transformEntities t entities
      mapM_ (render uniforms) es
      GLFW.swapBuffers window
      return (t, camera)
