module Entity where

import Graphics.GL.Core33
import Graphics.GL.Types
import Linear as L

import qualified Util.Model as U

data Entity = Entity
  { _model    :: U.VaoModel
  , _position :: V3 GLfloat
  , _rotation :: Quaternion GLfloat
  , _scale    :: GLfloat
  } deriving (Show)

rotate:: GLfloat -> V3 GLfloat -> Entity -> Entity
rotate delta dir entity = entity { _rotation = axisAngle dir delta }

translate:: V3 GLfloat -> Entity -> Entity
translate amount entity = entity { _position = (_position entity) + amount }
