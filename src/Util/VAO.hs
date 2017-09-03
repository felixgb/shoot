module Util.VAO where

import Graphics.GL.Core33

import Foreign

import Parser.ObjectParser

-- UNIFROMS HAVE TO BE LOADED BEFORE ANYTHING HAPPENS HERE!!!!!!!!!!!!!!!!!!!!
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
loadToVao (Object vs _ vns is) = do
  indicesP <- newArray is
  dataP <- newArray interwoven

  -- setup vertex array object
  vaoP <- malloc

  -- 1 is the number of vertex array object names to generate
  -- vaoP is the array where generate vertex array object names are to be
  -- stored
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP

  -- bind the VAO id so we are changing this one
  glBindVertexArray vao

  -- setup vertex buffer object and send it data
  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER interwovenSize (castPtr dataP) GL_STATIC_DRAW

  -- setup vertex normal buffer object and send it data

  -- set up an element buffer and send it data
  eboP <- malloc
  glGenBuffers 1 eboP
  ebo <- peek eboP
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

  -- position attribute is in 0
  -- 6 is the number from the start of a vertex to the start of the next
  -- vertex, eg x y z r g b x
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (6 * floatSize) nullPtr

  -- vertexNormal attribute is in 1
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (6 * floatSize) threeFloatOffset

  -- unbind this VAO
  glBindVertexArray 0
  return $ VaoModel vao (fromIntegral $ length is)
  where
    floatSize        = (fromIntegral $ sizeOf (0.0 :: GLfloat)) :: GLsizei
    interwoven       = interweave vs vns
    -- three float offset because vertex normals start after x y z
    threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3 * floatSize)
    indicesSize      = fromIntegral $ sizeOf (0 :: GLuint) * (length is)
    interwovenSize   = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length interwoven)

interweave :: [a] -> [a] -> [a]
interweave [] [] = []
interweave as xs = ah ++ xh ++ interweave at xt
  where
    (ah, at) = splitAt 3 as
    (xh, xt) = splitAt 3 xs
