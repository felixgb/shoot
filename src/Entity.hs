module Entity where

import Graphics.GL.Core33
import Linear hiding (rotate)

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

transformEntities :: GLfloat -> [Entity] -> [Entity]
transformEntities delta [e1, e2] = [r1, r2]
  where
    r1 = rotate (delta * 3) (V3 0 1 0) e1
    r2 = rotate delta (V3 1 0 0) e2
transformEntities _ _ = error "transforming not implemented"
