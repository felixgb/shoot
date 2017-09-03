module Entity.Entity where

import Data.Maybe (mapMaybe)
import Debug.Trace

import Graphics.GL.Core33
import Linear hiding (rotate, trace)

import Util.VAO
import Parser.ObjectParser
import Terrain.Terrain
import Entity.Collision

data Entity
  = Projectile AABB EntityInfo (V3 GLfloat)
  | Terrain EntityInfo
  | Teappot AABB EntityInfo
  deriving (Show)

data EntityInfo = EntityInfo
  { _vao         :: VaoModel
  , _position    :: V3 GLfloat
  , _rotation    :: Quaternion GLfloat
  , _scale       :: GLfloat
  , _renderMode  :: GLenum
  } deriving (Show)

getInfo :: Entity -> EntityInfo
getInfo e = case e of
  (Projectile _ i _) -> i
  (Terrain i)        -> i
  (Teappot _ i)      -> i

rotate:: GLfloat -> V3 GLfloat -> EntityInfo -> EntityInfo
rotate delta dir entity = entity { _rotation = axisAngle dir delta }

translate:: V3 GLfloat -> EntityInfo -> EntityInfo
translate amount entity = entity { _position = (_position entity) + amount }

getBB :: Entity -> Maybe AABB
getBB (Projectile bb _ _) = return bb
getBB (Terrain _)         = Nothing
getBB (Teappot bb _)      = return bb

transformEntity :: GLfloat -> [Entity] -> Entity -> Entity
transformEntity delta others entity = case entity of
  (Terrain _)                -> entity
  (Teappot bb info)          -> trace (show $ collided bb) (Teappot bb (rot info))
  (Projectile bb info front) -> trace (show $ collided bb) (Projectile bb (translate front info) front)
  where
    collided bb = any (insersectAABB bb) (mapMaybe getBB others)
    rot = rotate delta (V3 0 1 0)

loadEntityInfoFromFile :: FilePath -> V3 GLfloat -> Quaternion GLfloat -> GLfloat -> IO (EntityInfo, AABB)
loadEntityInfoFromFile path pos rot scale = do
  object <- parseObjectFromFile path
  vao    <- loadToVao object
  let bb = createAABB (_vertices object)
  return $ (EntityInfo vao pos rot scale GL_FILL, bb)

loadTerrain :: IO EntityInfo
loadTerrain = do
  vao <- flatTerrain >>= loadToVao
  return $ EntityInfo vao (V3 (-0) (-75) (-0)) (axisAngle (V3 0 0 0) 1) 1 GL_LINE

loadBulletInfo :: IO (EntityInfo, AABB)
loadBulletInfo = loadEntityInfoFromFile "resources/teapot.obj" (V3 0 0 0) (axisAngle (V3 0 0 0) 1) 0.2

newBullet :: (EntityInfo, AABB) -> V3 GLfloat -> V3 GLfloat -> Entity
newBullet (info, bb) pos front = Projectile bb (info { _position = pos}) front

initEntities :: IO [Entity]
initEntities = do
  t <- loadTerrain
  (e1, bb) <- loadEntityInfoFromFile "resources/teapot.obj" (V3 0 0 0) (axisAngle (V3 0 0 0) 1) 1
  return [Terrain t, Teappot bb e1]

scaleMatrix :: GLfloat -> M44 GLfloat
scaleMatrix n = V4
  (V4 n 0 0 0)
  (V4 0 n 0 0)
  (V4 0 0 n 0)
  (V4 0 0 0 1)
