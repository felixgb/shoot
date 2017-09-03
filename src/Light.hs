module Light where

import Graphics.GL.Core33

import Linear

data Light = Light
  { _lightPos :: V3 GLfloat
  , _lightColor :: V3 GLfloat
  }

centerLight :: Light
centerLight = Light (V3 1000 1000 1000) (V3 0.4 1 0.4)

moveLightTo :: V3 GLfloat -> Light -> Light
moveLightTo vec light = light { _lightPos = vec }
