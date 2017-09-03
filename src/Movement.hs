module Movement where

import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear hiding (trace)

data Camera = Camera
  { _pos   :: V3 GLfloat
  , _front :: V3 GLfloat
  , _up    :: V3 GLfloat
  , _accel :: GLfloat
  }

data MouseInfo = MouseInfo
  { _lastXY      :: Maybe (Double, Double)
  , _oldPitchYaw :: (Double, Double)
  , _frontVec    :: V3 GLfloat
  }

type KeysRef = IORef (Set GLFW.Key)

type MouseRef = IORef MouseInfo

data MovementRefs = MovementRefs
  { _mouseRef :: MouseRef
  , _keysRef  :: KeysRef
  }

type ClickRef = IORef (Set GLFW.MouseButton)

initMouse :: MouseInfo
initMouse = MouseInfo Nothing (0, (-90)) (V3 0 0 (-1))

initCamera :: Camera
initCamera = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0) 0

initMovementRefs :: IO MovementRefs
initMovementRefs = MovementRefs <$> newIORef initMouse <*> newIORef Set.empty

initClickRef :: IO ClickRef
initClickRef = newIORef Set.empty

keyCallback :: KeysRef -> GLFW.KeyCallback
keyCallback ref window key _ keyState _ = do
    case keyState of
      GLFW.KeyState'Pressed -> modifyIORef ref (Set.insert key)
      GLFW.KeyState'Released -> modifyIORef ref (Set.delete key)
      _ -> return ()
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

mouseCallback :: MouseRef -> GLFW.CursorPosCallback
mouseCallback ref _ xPos yPos = modifyIORef ref $ \info -> (mouseFunc xPos yPos info)

clickCallback :: ClickRef -> GLFW.MouseButtonCallback
clickCallback ref window button action mods = do
  case action of
    GLFW.MouseButtonState'Pressed -> modifyIORef ref (Set.insert button)
    GLFW.MouseButtonState'Released -> modifyIORef ref (Set.delete button)
    _ -> return ()

mouseFunc :: Double -> Double -> MouseInfo -> MouseInfo
mouseFunc xPos yPos oldInfo = MouseInfo (Just (xPos, yPos)) (newPitch, newYaw) front
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
    front              = normalize $ V3 (cos yawR * cos pitchR) (sin pitchR) (sin yawR * cos pitchR)

keyFunc :: GLfloat -> Camera -> Set GLFW.Key -> Camera
keyFunc speed cam@(Camera pos front up accel) keyset = cam { _pos = pos ^+^ (speed *^ normalize moveVector), _accel = newAccel }
  where
    modCam key (vec, a) = case key of
      GLFW.Key'W -> (vec ^+^ front, clamp (a + 0.02) (-0.2) (0.2))
      GLFW.Key'S -> (vec ^-^ front, clamp (a - 0.02) (-0.2) (0.2))
      GLFW.Key'Q -> (vec ^+^ up, a)
      GLFW.Key'E -> (vec ^-^ up, a)
      GLFW.Key'A -> (vec ^-^ normalize (cross front up), a)
      GLFW.Key'D -> (vec ^+^ normalize (cross front up), a)
      _ -> (vec, a)
    (moveVector, newAccel) = Set.foldr modCam ((V3 0 0 0), accel) keyset


clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp x lower upper
  | x < lower = lower
  | x > upper = upper
  | otherwise = x

toViewMatrix :: Camera -> M44 GLfloat
toViewMatrix (Camera pos front up accel) = lookAt pos (pos ^+^ front) up

toZero :: GLfloat -> GLfloat
toZero n
  | n > 0.001  = n - 0.02
  | n < -0.001  = n + 0.02
  | otherwise = n

smoothstep e0 e1 x = x' * x' * (3 - 2 * x')
  where x' = clamp ((x - e0) / (e1 - e0)) 0.0 1.0

updateCamera :: Set GLFW.Key -> MouseInfo -> Camera -> GLfloat -> GLfloat -> Camera
updateCamera keys mouse oldCamera elapsedTime currentTime = cameraTemp
  { _front = _frontVec mouse + V3 0 (-(accel)) 0
  , _accel = accel
  }
  where
    deltaTime   = currentTime - elapsedTime
    cameraSpeed = deltaTime * 20 + (2 * (abs $ _accel oldCamera))
    cameraTemp  = keyFunc cameraSpeed oldCamera keys
    smooth x = (smoothstep 0 0.3 x) * 0.3
    accel = if keys /= Set.empty then _accel cameraTemp else (toZero $ _accel cameraTemp)
