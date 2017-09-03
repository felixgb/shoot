module Entity where

import Graphics.GL.Core33
import Linear hiding (rotate)

import Util.VAO
import Parser.ObjectParser
import Terrain.Terrain

data Entity
  = Projectile EntityInfo (V3 GLfloat)
  | Terrain EntityInfo
  | Teappot EntityInfo
  deriving (Show)

data EntityInfo = EntityInfo
  { _vao        :: VaoModel
  , _position   :: V3 GLfloat
  , _rotation   :: Quaternion GLfloat
  , _scale      :: GLfloat
  , _renderMode :: GLenum
  } deriving (Show)

getInfo :: Entity -> EntityInfo
getInfo e = case e of
  (Projectile i _) -> i
  (Terrain i)      -> i
  (Teappot i )     -> i

rotate:: GLfloat -> V3 GLfloat -> EntityInfo -> EntityInfo
rotate delta dir entity = entity { _rotation = axisAngle dir delta }

translate:: V3 GLfloat -> EntityInfo -> EntityInfo
translate amount entity = entity { _position = (_position entity) + amount }

transformEntity :: GLfloat -> Entity -> Entity
transformEntity delta entity = case entity of
  (Terrain info) -> entity
  (Teappot info) -> Teappot (rot info)
  (Projectile info front) -> Projectile (translate front info) front
  where
    rot = rotate delta (V3 0 1 0)

loadEntityInfoFromFile :: FilePath -> V3 GLfloat -> Quaternion GLfloat -> GLfloat -> IO EntityInfo
loadEntityInfoFromFile path pos rot scale = do
  vao <- parseObjectFromFile path >>= loadToVao
  return $ EntityInfo vao pos rot scale GL_FILL

loadTerrain :: IO EntityInfo
loadTerrain = do
  vao <- flatTerrain >>= loadToVao
  return $ EntityInfo vao (V3 (-0) (-75) (-0)) (axisAngle (V3 0 0 0) 1) 1 GL_LINE

loadBulletInfo :: IO EntityInfo
loadBulletInfo = loadEntityInfoFromFile "resources/teapot.obj" (V3 0 0 0) (axisAngle (V3 0 0 0) 1) 0.2

newBullet :: EntityInfo -> V3 GLfloat -> V3 GLfloat -> Entity
newBullet info pos front = Projectile (info { _position = pos}) front

initEntities :: IO [Entity]
initEntities = do
  t <- loadTerrain
  e1 <- loadEntityInfoFromFile "resources/teapot.obj" (V3 0 0 0) (axisAngle (V3 0 0 0) 1) 1
  return [Terrain t, Teappot e1]

scaleMatrix :: GLfloat -> M44 GLfloat
scaleMatrix n = V4
  (V4 n 0 0 0)
  (V4 0 n 0 0)
  (V4 0 0 n 0)
  (V4 0 0 0 1)
