module Util.Model where

import Data.Sequence (Seq)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

import Graphics.GL.Core33

import Foreign

data Object = Object
  { vertices :: Seq GLfloat
  , colors   :: Seq GLfloat
  , indices  :: Seq GLuint
  } deriving (Show)

data VaoModel = VaoModel
  { _vaoID       :: GLuint
  , _numVertices :: GLsizei
  } deriving (Show)

-- | A VAO stores data about a 3d model. They have a number of slots called
-- Attribute Lists. You store different sets of data into these attribute
-- lists. ex: 0: vertex positions, 1: vertex colors...
--
-- These data sets are called VBOs. A VBO is an array of any kind of data,
-- positions, normals, etc
--
-- Each VAO has a unique ID, so you can reference it any time sing its ID.  You
-- can use the id of a VAO to render an object
loadToVao :: Object -> IO VaoModel
loadToVao (Object vq _ iq) = do
  verticesP <- newArray $ toList vq
  indicesP <- newArray $ toList iq

  -- setup vertex array object
  vaoP <- malloc

  -- 1 is the number of vertex array object names to generate
  -- vaoP is the array where generate vertex array object names are to be stored
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP

  -- bind the VAO id so we are changing this one
  glBindVertexArray vao

  -- setup vertex buffer object and send it data
  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

  -- set up an element buffer and send it data
  eboP <- malloc
  glGenBuffers 1 eboP
  ebo <- peek eboP
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

  -- position attribute is in 0
  -- 6 is the number from the start of a vertex to the start of the next
  -- vertex, eg x y z r g b x
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (3 * floatSize) nullPtr
  glEnableVertexAttribArray 0

  -- color attribute is in 1
  -- glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (6 * floatSize) threeFloatOffset
  -- glEnableVertexAttribArray 1

  -- unbind this VAO
  glBindVertexArray 0
  return $ VaoModel vao (fromIntegral $ Seq.length iq)
  where
    floatSize        = (fromIntegral $ sizeOf (0.0 :: GLfloat)) :: GLsizei
    -- three float offset because colors start after x y z
    -- threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3 * floatSize)
    verticesSize     = fromIntegral $ sizeOf (0.0 :: GLfloat) * (Seq.length vq)
    indicesSize      = fromIntegral $ sizeOf (0 :: GLuint) * (Seq.length iq)
    -- interwoven       = interweave vs cs

interweave :: [a] -> [a] -> [a]
interweave [] [] = []
interweave as xs = ah ++ xh ++ interweave at xt
  where
    (ah, at) = splitAt 3 as
    (xh, xt) = splitAt 3 xs
