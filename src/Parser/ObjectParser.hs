module Parser.ObjectParser where

import Control.Monad.Except
import Control.Monad.State
import Control.Exception

import Graphics.GL.Types

import Parser.Lexer
import qualified Util.Model as U
import qualified Util.Common as U

type ObjectState = StateT U.Object U.ThrowsError

parseObject :: [Token] -> ObjectState ()
parseObject [] = return ()
parseObject tks = case head tks of
  TkV  -> parseVertex tks >>= parseObject
  TkVN -> parseVertexNormal tks >>= parseObject
  TkF  -> parseFace tks >>= parseObject
  _    -> parseObject (tail tks)

insertVertex :: [GLfloat] -> U.Object -> U.Object
insertVertex vs obj = obj { U.vertices = vs ++ U.vertices obj }

insertVertexNormal :: [GLfloat] -> U.Object -> U.Object
insertVertexNormal vns obj = obj { U.vertexNormals = vns ++ U.vertexNormals obj }

insertIndex :: [GLuint] -> U.Object -> U.Object
insertIndex is obj = obj { U.indices = is ++ U.indices obj }

parseVertex :: [Token] -> ObjectState [Token]
parseVertex (TkV : TkFloating x : TkFloating y : TkFloating z : ts) = modify (insertVertex [x, y, z]) >> return ts
parseVertex _ = throwError $ U.ParseError

parseVertexNormal :: [Token] -> ObjectState [Token]
parseVertexNormal (TkVN : TkFloating x : TkFloating y : TkFloating z : ts) = modify (insertVertexNormal [x, y, z]) >> return ts
parseVertexNormal _ = throwError $ U.ParseError

parseFace :: [Token] -> ObjectState [Token]
parseFace (TkF : ts) = parseIndex ts >>= parseIndex >>= parseIndex
parseFace _ = throwError $ U.ParseError

parseIndex :: [Token] -> ObjectState [Token]
parseIndex (TkInt i : TkSlash : _ : _ : _ : ts) = modify (insertIndex [i]) >> return ts
parseIndex (TkInt i : ts) = modify (insertIndex [i]) >> return ts
parseIndex _ = throwError $ U.ParseError

runParser :: ObjectState a -> Either U.ShootError U.Object
runParser parser = runExcept $ execStateT parser emptyObj
    where emptyObj = U.Object [] [] [] []

parseObjectFromFile :: FilePath -> IO U.Object
parseObjectFromFile path = do
  tokens <- scan <$> readFile path
  case runParser (parseObject tokens) of
    Left err -> throwIO $ err
    Right (U.Object a b c d) -> return $ U.Object (reverse a) (reverse b) (reverse c) (reverse d)
