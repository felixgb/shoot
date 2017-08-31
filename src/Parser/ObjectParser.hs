module Parser.ObjectParser where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Control.Monad.Except
import Control.Monad.State
import Control.Exception

import Graphics.GL.Types

import Parser.Lexer
import qualified Util.Model as U
import qualified Util.Common as U

type ObjectState = StateT U.Object U.ThrowsError

runParser :: ObjectState a -> Either U.ShootError U.Object
runParser parser = runExcept $ execStateT parser emptyObj
    where emptyObj = U.Object Seq.empty Seq.empty Seq.empty

parseObject :: [Token] -> ObjectState ()
parseObject [] = return ()
parseObject tks = case head tks of
  TkV -> parseVertex tks >>= parseObject
  TkF -> parseFace tks >>= parseObject
  _   -> parseObject (tail tks)

insertVertex :: Seq GLfloat -> U.Object -> U.Object
insertVertex vs obj = obj { U.vertices = U.vertices obj >< vs }

insertIndex :: Seq GLuint -> U.Object -> U.Object
insertIndex is obj = obj { U.indices = U.indices obj >< is }

parseVertex :: [Token] -> ObjectState [Token]
parseVertex (TkV : TkFloating x : TkFloating y : TkFloating z : ts) = modify (insertVertex $ Seq.fromList [x, y, z]) >> return ts
parseVertex _ = throwError $ U.ParseError

parseFace :: [Token] -> ObjectState [Token]
parseFace (TkF : ts) = parseIndex ts >>= parseIndex >>= parseIndex
parseFace _ = throwError $ U.ParseError

parseIndex :: [Token] -> ObjectState [Token]
parseIndex (TkInt i : TkSlash : _ : _ : _ : ts) = modify (insertIndex $ Seq.fromList [i]) >> return ts
parseIndex (TkInt i : ts) = modify (insertIndex $ Seq.fromList [i]) >> return ts
parseIndex _ = throwError $ U.ParseError

parseObjectFromFile :: FilePath -> IO U.Object
parseObjectFromFile path = do
  tokens <- scan <$> readFile path
  case runParser (parseObject tokens) of
    Left err -> throwIO $ err
    Right ok -> return ok
