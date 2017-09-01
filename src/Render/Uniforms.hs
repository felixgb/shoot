module Render.Uniforms where

import Foreign hiding (rotate)
import Foreign.C.String

import Graphics.GL.Core33
import Linear hiding (rotate)

import Render.Shaders

data UniformM44 = UniformM44
  { _locM4 :: GLint
  , _memM4:: Ptr (M44 GLfloat)
  }

data UniformV3 = UniformV3
  { _locV3 :: GLint
  , _memV3 :: Ptr (V3 GLfloat)
  }

data Uniforms = Uniforms
  { _model       :: UniformM44
  , _view        :: UniformM44
  , _proj        :: UniformM44
  , _ulightPos   :: UniformV3
  , _ulightColor :: UniformV3
  }

applyUniformM44 :: M44 GLfloat -> UniformM44 -> IO ()
applyUniformM44 mat (UniformM44 loc mem) = do
  poke mem (transpose mat)
  glUniformMatrix4fv loc 1 GL_FALSE (castPtr mem)

applyUniformV3 :: V3 GLfloat -> UniformV3 -> IO ()
applyUniformV3 vec (UniformV3 loc mem) = do
  poke mem vec
  glUniform3fv loc 1 (castPtr mem)

newUniformM44 :: String -> GLuint ->  IO UniformM44
newUniformM44 name prog = UniformM44 <$> loc <*> malloc
  where loc = withCString name (glGetUniformLocation prog)

newUniformV3 :: String -> GLuint ->  IO UniformV3
newUniformV3 name prog = UniformV3 <$> loc <*> malloc
  where loc = withCString name (glGetUniformLocation prog)

initUniforms :: IO Uniforms
initUniforms = do
  shaderProgram <- initShaders
    [ (GL_VERTEX_SHADER, "src/glsl/vertex.shader")
    , (GL_FRAGMENT_SHADER, "src/glsl/fragment.shader")
    ]
  model      <- newUniformM44 "model" shaderProgram
  view       <- newUniformM44 "view" shaderProgram
  projection <- newUniformM44 "projection" shaderProgram
  lightPos   <- newUniformV3 "lightPos" shaderProgram
  lightColor <- newUniformV3 "lightColor" shaderProgram
  return Uniforms
    { _model      = model
    , _view       = view
    , _proj       = projection
    , _ulightPos   = lightPos
    , _ulightColor = lightColor
    }
