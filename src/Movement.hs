module Movement where

import Control.Monad (when, unless, forever)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear

data Camera = Camera
  { _pos   :: V3 GLfloat
  , _front :: V3 GLfloat
  , _up    :: V3 GLfloat
  }

data MouseInfo = MouseInfo
  { _lastXY      :: Maybe (Double, Double)
  , _oldPitchYaw :: (Double, Double)
  , _frontVec    :: V3 GLfloat
  }

type KeysRef = IORef (Set GLFW.Key)

type MouseRef = IORef MouseInfo

keyCallback :: KeysRef -> GLFW.KeyCallback
keyCallback ref window key scanCode keyState modKeys = do
    case keyState of
      GLFW.KeyState'Pressed -> modifyIORef ref (Set.insert key)
      GLFW.KeyState'Released -> modifyIORef ref (Set.delete key)
      _ -> return ()
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

mouseCallback :: MouseRef -> GLFW.CursorPosCallback
mouseCallback ref window xPos yPos = modifyIORef ref $ \info -> (mouseFunc xPos yPos info)

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

updateCamera :: GLfloat -> Camera -> Set GLFW.Key -> Camera
updateCamera speed cam@(Camera pos front up) keyset = cam { _pos = pos ^+^ (speed *^ normalize moveVector) }
  where
    modCam key vec = case key of
      GLFW.Key'W -> vec ^+^ front
      GLFW.Key'S -> vec ^-^ front
      GLFW.Key'Q -> vec ^+^ up
      GLFW.Key'E -> vec ^-^ up
      GLFW.Key'A -> vec ^-^ normalize (cross front up)
      GLFW.Key'D -> vec ^+^ normalize (cross front up)
      _ -> vec
    moveVector = Set.foldr modCam (V3 0 0 0) keyset

toViewMatrix :: Camera -> M44 GLfloat
toViewMatrix (Camera pos front up) = lookAt pos (pos ^+^ front) up

initMouse :: MouseInfo
initMouse = MouseInfo Nothing (0, (-90)) (V3 0 0 (-1))

initCamera :: Camera
initCamera = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0)
