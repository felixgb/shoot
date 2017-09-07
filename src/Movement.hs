module Movement where

import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')
import Data.Set (Set)
import Debug.Trace
import qualified Data.Set as Set

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear hiding (trace)

data Camera = Camera
  { _pos   :: V3 GLfloat
  , _pitch :: GLfloat
  , _yaw   :: GLfloat
  , _roll  :: GLfloat
  }

data MouseInfo = MouseInfo
  { _lastXY      :: Maybe (Double, Double)
  , _oldPitchYaw :: (Double, Double)
  , _pitchYaw    :: (GLfloat, GLfloat)
  }

type KeysRef = IORef (Set GLFW.Key)

type MouseRef = IORef MouseInfo

data MovementRefs = MovementRefs
  { _mouseRef :: MouseRef
  , _keysRef  :: KeysRef
  }

type ClickRef = IORef (Set GLFW.MouseButton)

initMouse :: MouseInfo
initMouse = MouseInfo Nothing (0, 0) (0, 0)

initCamera :: Camera
initCamera = Camera (V3 0 0 3) 0 0 0

initMovementRefs :: IO MovementRefs
initMovementRefs = MovementRefs <$> newIORef initMouse <*> newIORef Set.empty

initClickRef :: IO ClickRef
initClickRef = newIORef Set.empty

keyCallback :: KeysRef -> GLFW.KeyCallback
keyCallback ref window key _ keyState _ = do
    case keyState of
      GLFW.KeyState'Pressed  -> modifyIORef ref (Set.insert key)
      GLFW.KeyState'Released -> modifyIORef ref (Set.delete key)
      _                      -> return ()
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

mouseCallback :: MouseRef -> GLFW.CursorPosCallback
mouseCallback ref _ xPos yPos = modifyIORef ref $ \info -> (mouseFunc xPos yPos info)

clickCallback :: ClickRef -> GLFW.MouseButtonCallback
clickCallback ref window button action mods = do
  case action of
    GLFW.MouseButtonState'Pressed  -> modifyIORef ref (Set.insert button)
    GLFW.MouseButtonState'Released -> modifyIORef ref (Set.delete button)

mouseFunc :: Double -> Double -> MouseInfo -> MouseInfo
mouseFunc xPos yPos oldInfo = MouseInfo (Just (xPos, yPos)) (newPitch, newYaw) (-pitchR, yawR)
  where
    (lastX, lastY)     = fromMaybe (xPos, yPos) (_lastXY oldInfo)
    sensitivity        = 0.5
    xOffset            = (xPos - lastX) * sensitivity
    yOffset            = (lastY - yPos) * sensitivity
    (oldPitch, oldYaw) = _oldPitchYaw oldInfo
    newYaw             = (oldYaw + xOffset) `mod'` 360.0
    newPitch           = min (max (oldPitch + yOffset) (-89)) 89
    toRadians          = realToFrac . (* (pi / 180)) :: Double -> GLfloat
    pitchR             = toRadians newPitch
    yawR               = toRadians newYaw

keyFunc :: GLfloat -> Camera -> Set GLFW.Key -> Camera
keyFunc speed cam@(Camera pos pitch yaw roll) keyset = cam { _pos = newPos, _roll = newRoll }
  where
    dx = sin yaw
    dz = cos yaw
    dy = sin pitch
    modCam key (vec, r) = case key of
      GLFW.Key'W -> (vec ^+^ (V3 (-dx) dy dz), r)
      GLFW.Key'S -> (vec ^+^ (V3 dx (-dy) (-dz)), r)
      GLFW.Key'A -> (vec ^+^ (V3 dz 0 dx), r)
      GLFW.Key'D -> (vec ^-^ (V3 dz 0 dx), r)
      GLFW.Key'Q -> (vec, r - 0.1)
      GLFW.Key'E -> (vec, r + 0.1)
      _ -> (vec, r)
    (newPos, newRoll) = Set.foldr modCam (pos, roll) keyset

clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp x lower upper
  | x < lower = lower
  | x > upper = upper
  | otherwise = x

toViewMatrix :: Camera -> M44 GLfloat
toViewMatrix (Camera pos pitch yaw roll) = rollM !*! pitchM !*! yawM !*! posM
  where
    posM   = transMatrix pos
    yawM   = rotYM yaw
    pitchM = rotXM pitch
    rollM  = rotZM roll

rotXM :: GLfloat -> M44 GLfloat
rotXM d = V4
  (V4 1 0       0          0)
  (V4 0 (cos d) (-(sin d)) 0)
  (V4 0 (sin d) (cos d)    0)
  (V4 0 0       0          1)

rotYM :: GLfloat -> M44 GLfloat
rotYM d = V4
  (V4 (cos d)    0 (sin d) 0)
  (V4 0          1 0       0)
  (V4 (-(sin d)) 0 (cos d) 0)
  (V4 0          0 0       1)

rotZM :: GLfloat -> M44 GLfloat
rotZM d = V4
  (V4 (cos d) (-(sin d)) 0 0)
  (V4 (sin d) (cos d)    0 0)
  (V4 0       0          1 0)
  (V4 0       0          0 1)

transMatrix :: V3 GLfloat -> M44 GLfloat
transMatrix (V3 x y z) = V4
  (V4 1 0 0 x)
  (V4 0 1 0 y)
  (V4 0 0 1 z)
  (V4 0 0 0 1)

sign :: GLfloat -> GLfloat
sign x = if x > 0 then 1 else -1

toZero :: GLfloat -> GLfloat
toZero n
  | n > 0.001  = n - 0.02
  | n < -0.001  = n + 0.02
  | otherwise = n

smoothstep :: GLfloat -> GLfloat -> GLfloat -> GLfloat
smoothstep e0 e1 x = x' * x' * (3 - 2 * x')
  where x' = clamp ((x - e0) / (e1 - e0)) 0.0 1.0

updateCamera :: Set GLFW.Key -> MouseInfo -> Camera -> GLfloat -> GLfloat -> Camera
updateCamera keys mouse oldCamera elapsedTime currentTime = cameraTemp
  { _pitch = fst $ _pitchYaw mouse
  , _yaw = snd $ _pitchYaw mouse
  }
  where
    deltaTime    = currentTime - elapsedTime
    cameraSpeed  = deltaTime * 20
    cameraTemp   = keyFunc cameraSpeed oldCamera keys
