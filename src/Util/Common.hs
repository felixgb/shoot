module Util.Common where

import Control.Monad.Except
import Control.Exception

data ShootError
  = WindowCreationError
  | ShaderCompileError String
  | ProgramLinkError String
  deriving (Show)

instance Exception ShootError

type ThrowsError = Except ShootError
