module Entity where

import Graphics.GL.Core33
import Linear hiding (rotate)

import Util.VAO
import Parser.ObjectParser
import Terrain.Terrain

data Entity = Entity
  { _vao      :: VaoModel
  , _position :: V3 GLfloat
  , _rotation :: Quaternion GLfloat
  , _scale    :: GLfloat
  } deriving (Show)

rotate:: GLfloat -> V3 GLfloat -> Entity -> Entity
rotate delta dir entity = entity { _rotation = axisAngle dir delta }

translate:: V3 GLfloat -> Entity -> Entity
translate amount entity = entity { _position = (_position entity) + amount }

transformEntities :: GLfloat -> [Entity] -> [Entity]
transformEntities delta (e1 : xs) = (rotate (delta * 3) (V3 0 1 0) e1) : xs
transformEntities _ _ = error "transforming not implemented"

loadEntityFromFile :: FilePath -> V3 GLfloat -> Quaternion GLfloat -> GLfloat -> IO Entity
loadEntityFromFile path pos rot scale = do
  vao <- parseObjectFromFile path >>= loadToVao
  return $ Entity vao pos rot scale

loadTerrain :: IO Entity
loadTerrain = do
  vao <- flatTerrain >>= loadToVao
  return $ Entity vao (V3 (-250) (-10) (-250)) (axisAngle (V3 0 0 0) 1) 1

initEntities :: IO [Entity]
initEntities = do
  t <- loadTerrain
  e1 <- loadEntityFromFile "resources/teapot.obj" (V3 0 0 0) (axisAngle (V3 0 0 0) 1) 1
  return [e1, t]
