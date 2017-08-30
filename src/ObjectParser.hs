module ObjectParser where

import Graphics.GL.Types

import Control.Monad.State

import Lexer
import qualified Util.Model as U

type ObjectState = State U.Object

parseObjectFromFile :: FilePath -> IO U.Object
parseObjectFromFile path = do
  tokens <- scan <$> readFile path
  return $ execState (parseObject tokens) emptyObj
    where emptyObj = U.Object [] [] []

parseObject :: [Token] -> ObjectState ()
parseObject [] = return ()
parseObject tks = case head tks of
  TkV -> parseVertex tks >>= parseObject
  TkF -> parseFace tks >>= parseObject
  _   -> parseObject (tail tks)

insertVertex :: [GLfloat] -> U.Object -> U.Object
insertVertex vs obj = obj { U.vertices = U.vertices obj ++ vs }

insertIndex :: [GLuint] -> U.Object -> U.Object
insertIndex is obj = obj { U.indices = U.indices obj ++ is }

parseVertex :: [Token] -> ObjectState [Token]
parseVertex (TkV : TkFloating x : TkFloating y : TkFloating z : ts) = modify (insertVertex [x, y, z]) >> return ts

parseFace :: [Token] -> ObjectState [Token]
parseFace (TkF : ts) = parseIndex ts >>= parseIndex >>= parseIndex

parseIndex :: [Token] -> ObjectState [Token]
parseIndex (TkInt i : _ : _ : _ : _ : ts) = modify (insertIndex [i]) >> return ts
