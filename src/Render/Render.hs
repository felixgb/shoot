module Render.Render where

import Control.Monad.Loops (iterateM_)
import Control.Monad (forM_)
import Data.IORef (readIORef)
import Data.Bits ((.|.))
import qualified Data.Set as Set
import Foreign hiding (rotate)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear hiding (rotate)

import Entity.Entity
import Entity.Collision
import Light
import Movement
import Util.VAO
import Render.Uniforms
import Window

applyLights :: Uniforms -> [Light] -> IO ()
applyLights uniforms lights = forM_ lights $ \(Light pos color) -> do
  applyUniformV3 pos (_ulightPos uniforms)
  applyUniformV3 color (_ulightColor uniforms)

applyProjection :: Uniforms -> GLFW.Window -> IO ()
applyProjection uniforms window = do
  (x, y) <- GLFW.getFramebufferSize window
  let projM = perspective 45 (fromIntegral x / fromIntegral y) 0.1 1000.0
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

applyClick :: Camera -> (EntityInfo, AABB) -> ClickRef -> IO [Entity]
applyClick cam info ref = do
  buttons <- readIORef ref
  -- check dif lasttime current time is above some thresh to limit ROF
  case GLFW.MouseButton'1 `Set.member` buttons of
    True -> return [newBullet info (_pos cam) (_front cam)]
    False -> return []

renderInfo :: Uniforms -> EntityInfo -> IO ()
renderInfo uniforms (EntityInfo (VaoModel vaoID numVertices) pos rot scale mode) = do
  let modelM = (mkTransformation rot pos) !*! (scaleMatrix scale)
  applyUniformM44 modelM (_model uniforms)
  glPolygonMode GL_FRONT_AND_BACK mode
  glBindVertexArray vaoID
  -- vertices in attr 0
  glEnableVertexAttribArray 0
  -- vertex normals in attr 1
  glEnableVertexAttribArray 1
  glDrawElements GL_TRIANGLES numVertices GL_UNSIGNED_INT nullPtr
  glDisableVertexAttribArray 0
  glDisableVertexAttribArray 1
  glBindVertexArray 0

render :: Uniforms -> Entity -> IO ()
render uniforms entity = case entity of
  (Projectile _ i _) -> renderInfo uniforms i
  (Terrain i)        -> renderInfo uniforms i
  (Teappot _ i)      -> renderInfo uniforms i
  (Player _)         -> return ()

initDisplay :: GLFW.Window -> Uniforms -> MovementRefs -> ClickRef -> [Entity] -> [Light] -> IO ()
initDisplay window uniforms moveRef clickRef entities lights = do
  applyProjection uniforms window
  applyLights uniforms lights
  bulletInfo <- loadBulletInfo
  flip iterateM_ (0.0, initCamera, entities) $ \(lastTime, oldCamera, es) -> do
    shouldTerminate window
    GLFW.pollEvents
    glClearColor 0.0 0.0 0.0 1.0
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    camera   <- applyViewMove uniforms moveRef oldCamera lastTime
    t        <- (maybe 0 realToFrac <$> GLFW.getTime) :: IO GLfloat
    shootems <- applyClick camera bulletInfo clickRef
    let es'  = shootems ++ es
    -- ... get the current entity and check collisions against all else
    mapM_ (putStrLn . show) (collisions es')
    let es'' = map (transformEntity t camera es') es'
    mapM_ (render uniforms) es'
    GLFW.swapBuffers window
    return (t, camera, es'')
