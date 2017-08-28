module Util.Shaders
  ( initShaders
  ) where

import Control.Monad (when, unless, forever)
import Control.Exception
import System.Exit
import Foreign
import Foreign.C.String (newCAStringLen)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

import qualified Util.Common as U

-- | Get the string from the program and throw the error with the data
handleError getLog err = do
  let infoLength = 512
  resultP <- malloc
  infoLog <- mallocArray (fromIntegral infoLength)
  getLog (fromIntegral infoLength) resultP infoLog
  result <- fromIntegral <$> peek resultP
  logBytes <- peekArray result infoLog
  throwIO $ err (map (toEnum . fromEnum) logBytes)

-- | Given a shader type and the path of the shader, return the shader ID
loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderType source = do
  -- create new shader object
  shader <- glCreateShader shaderType

  shaderSource <- readFile source

  -- assign the source to the shader object
  (sourceP, len) <- newCAStringLen shaderSource
  linesPtrsPtr <- newArray [sourceP]
  lengthsPtr <- newArray [fromIntegral len]
  glShaderSource shader 1 linesPtrsPtr lengthsPtr

  -- compile
  glCompileShader shader
  successP <- malloc
  glGetShaderiv shader GL_COMPILE_STATUS successP
  success <- peek successP
  case success of
    0 -> handleError (glGetShaderInfoLog shader) U.ShaderCompileError
    _ -> return shader

-- | Given a list of shader ids, link them to GL program
linkShaders :: [GLuint] -> IO GLuint
linkShaders shaders = do
  shaderProgram <- glCreateProgram
  mapM_ (glAttachShader shaderProgram) shaders
  glLinkProgram shaderProgram
  linkSuccessP <- malloc
  glGetProgramiv shaderProgram GL_LINK_STATUS linkSuccessP
  linkSuccess <- peek linkSuccessP
  case linkSuccess of
    0 -> handleError (glGetProgramInfoLog shaderProgram) U.ProgramLinkError
    _ -> mapM_ glDeleteShader shaders >> return shaderProgram

initShaders :: [(GLenum, FilePath)] -> IO GLuint
initShaders shaders = do
  shaderIDs <- mapM (\(ty, path) -> loadShader ty path) shaders
  linkShaders shaderIDs
