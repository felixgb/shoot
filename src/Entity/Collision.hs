module Entity.Collision where

import Graphics.GL.Core33
import Linear

-- An axis aligned bounding box forms the two far corners of an object
data AABB = AABB
  { _aabbMin :: V3 GLfloat
  , _aabbMax ::  V3 GLfloat
  } deriving (Show, Eq)

insersectAABB :: AABB -> AABB -> Bool
insersectAABB (AABB aMin aMax) (AABB bMin bMax) =
  (aMinX <= bMaxX && aMaxX >= bMinX) &&
  (aMinY <= bMaxY && aMaxY >= bMinY) &&
  (aMinZ <= bMaxZ && aMaxZ >= bMinZ)
  where
    (V3 aMinX aMinY aMinZ) = aMin
    (V3 bMinX bMinY bMinZ) = bMin
    (V3 aMaxX aMaxY aMaxZ) = aMax
    (V3 bMaxX bMaxY bMaxZ) = bMax

--translateAABB :: V3 GLfloat -> AABB -> AABB

chunk :: Int -> [a] -> [V3 a]
chunk _ [] = []
chunk n ls = V3 a b c : chunk n lt
  where ([a, b, c], lt) = splitAt n ls

compElems :: Ord a => (a -> a -> a) -> V3 a -> V3 a -> V3 a
compElems f (V3 a b c) (V3 x y z) = V3 (f a x) (f b y) (f c z)

compCoords :: (Ord a) => V3 a -> (V3 a, V3 a) -> (V3 a, V3 a)
compCoords newCoords (oldMin, oldMax) = (compElems min oldMin newCoords, compElems max oldMax newCoords)

createAABB :: [GLfloat] -> AABB
createAABB vs = AABB aMax aMin
  where (aMax, aMin) = foldr compCoords (V3 0 0 0, V3 0 0 0) (chunk 3 vs)
