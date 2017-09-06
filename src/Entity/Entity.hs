module Entity.Entity where

import Data.Maybe (mapMaybe)

import Graphics.GL.Core33
import Linear hiding (rotate, trace)

import Util.VAO
import Parser.ObjectParser
import Terrain.Terrain
import Entity.Collision
import Movement

data Entity
  = Projectile AABB EntityInfo (V3 GLfloat)
  | Terrain EntityInfo
  | Teappot AABB EntityInfo
  | Player AABB
  deriving (Show)

data EntityInfo = EntityInfo
  { _vao         :: VaoModel
  , _position    :: V3 GLfloat
  , _rotation    :: Quaternion GLfloat
  , _scale       :: GLfloat
  , _renderMode  :: GLenum
  } deriving (Show)

class Transformable a where
  translate :: V3 GLfloat -> a -> a
  rotate    :: GLfloat -> V3 GLfloat -> a -> a

-- transform and rotate functions should take an entity and move its BB as well
instance Transformable EntityInfo where
  translate amount entity = entity { _position = (_position entity) + amount }
  rotate amount dir entity = entity { _rotation = axisAngle dir amount }

instance Transformable AABB where
  translate amount (AABB bbMin bbMax) = AABB (bbMin + amount) (bbMax + amount)
  -- makes no sense to rotate an 'axis aligned' bounding box
  rotate _ _ bb = bb

instance Transformable Entity where
  translate amount entity = case entity of
    (Terrain _)                -> entity
    (Teappot bb info)          -> Teappot (translate amount bb) (translate amount info)
    (Projectile bb info front) -> Projectile (translate amount bb) (translate amount info) front

  rotate amount dir entity = case entity of
    (Terrain _)                -> entity
    (Teappot bb info)          -> Teappot (rotate amount dir bb) (rotate amount dir info)
    (Projectile bb info front) -> Projectile (rotate amount dir bb) (rotate amount dir info) front

getBB :: Entity -> Maybe AABB
getBB (Projectile bb _ _) = return bb
getBB (Terrain _)         = Nothing
getBB (Teappot bb _)      = return bb
getBB (Player bb)         = return bb

transformEntity :: GLfloat -> Camera -> [Entity] -> Entity -> Entity
transformEntity delta camera others entity = case entity of
  (Terrain _)                -> entity
  (Teappot bb info)          -> rot entity
  (Projectile bb info front) -> translate front entity
  (Player bb)                -> Player $ AABB (_pos camera) (_pos camera)
  where
    rot = rotate delta (V3 0 1 0)

hasCollided :: Entity -> Entity -> Maybe Collision
hasCollided a b = do
  aBB <- getBB a
  bBB <- getBB b
  if insersectAABB aBB bBB then return $ Collision a b else Nothing

data Collision = Collision Entity Entity
  deriving (Show)

collisions :: [Entity] -> [Collision]
collisions [] = []
collisions (e : es) = (mapMaybe (hasCollided e) es) ++ collisions es

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

initPlayer :: Entity
initPlayer = Player $ AABB (V3 0 0 0) (V3 0 0 0)

initEntities :: IO [Entity]
initEntities = do
  t <- loadTerrain
  (e1, bb) <- loadEntityInfoFromFile "resources/teapot.obj" (V3 0 0 0) (axisAngle (V3 0 0 0) 1) 1
  return [Terrain t, Teappot bb e1, initPlayer]

scaleMatrix :: GLfloat -> M44 GLfloat
scaleMatrix n = V4
  (V4 n 0 0 0)
  (V4 0 n 0 0)
  (V4 0 0 n 0)
  (V4 0 0 0 1)
