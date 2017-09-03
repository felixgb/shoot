module Parser.ObjectParser where

import Control.Monad.Except
import Control.Monad.State
import Control.Exception

import Graphics.GL.Types

import Parser.Lexer
import Util.Common

data Object = Object
  { _vertices      :: [GLfloat]
  , _colors        :: [GLfloat]
  , _vertexNormals :: [GLfloat]
  , _indices       :: [GLuint]
  } deriving (Show)

type ObjectState = StateT Object ThrowsError

parseObject :: [Token] -> ObjectState ()
parseObject [] = return ()
parseObject tks = case head tks of
  TkV  -> parseVertex tks >>= parseObject
  TkVN -> parseVertexNormal tks >>= parseObject
  TkF  -> parseFace tks >>= parseObject
  _    -> parseObject (tail tks)

insertVertex :: [GLfloat] -> Object -> Object
insertVertex vs obj = obj { _vertices = vs ++ _vertices obj }

insertVertexNormal :: [GLfloat] -> Object -> Object
insertVertexNormal vns obj = obj { _vertexNormals = vns ++ _vertexNormals obj }

insertIndex :: [GLuint] -> Object -> Object
insertIndex is obj = obj { _indices = is ++ _indices obj }

parseVertex :: [Token] -> ObjectState [Token]
parseVertex (TkV : TkFloating x : TkFloating y : TkFloating z : ts) = modify (insertVertex [x, y, z]) >> return ts
parseVertex _ = throwError $ ParseError

parseVertexNormal :: [Token] -> ObjectState [Token]
parseVertexNormal (TkVN : TkFloating x : TkFloating y : TkFloating z : ts) = modify (insertVertexNormal [x, y, z]) >> return ts
parseVertexNormal _ = throwError $ ParseError

parseFace :: [Token] -> ObjectState [Token]
parseFace (TkF : ts) = parseIndex ts >>= parseIndex >>= parseIndex
parseFace _ = throwError $ ParseError

parseIndex :: [Token] -> ObjectState [Token]
parseIndex (TkInt i : TkSlash : _ : _ : _ : ts) = modify (insertIndex [i]) >> return ts
parseIndex (TkInt i : ts) = modify (insertIndex [i]) >> return ts
parseIndex _ = throwError $ ParseError

runParser :: ObjectState a -> Either ShootError Object
runParser parser = runExcept $ execStateT parser emptyObj
    where emptyObj = Object [] [] [] []

parseObjectFromFile :: FilePath -> IO Object
parseObjectFromFile path = do
  tokens <- scan <$> readFile path
  case runParser (parseObject tokens) of
    Left err -> throwIO $ err
    Right (Object a b c d) -> return $ Object (reverse a) (reverse b) (reverse c) (reverse d)
