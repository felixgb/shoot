module Light where

import Graphics.GL.Core33

import Linear

data Light = Light
  { _lightPos :: V3 GLfloat
  , _lightColor :: V3 GLfloat
  }

centerLight :: Light
centerLight = Light (V3 3 10 0) (V3 1 1 1)
