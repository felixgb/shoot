module Light where

import Graphics.GL.Core33

import Linear

data Light = Light
  { _lightPos :: V3 GLfloat
  , _lightColor :: V3 GLfloat
  }
