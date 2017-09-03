module Entity.Collision where

import Graphics.GL.Core33
import Linear

-- An axis aligned bounding box forms the two far corners of an object
data AABB = AABB
  { _aabbMin :: (GLfloat, GLfloat, GLfloat)
  , _aabbMax :: (GLfloat, GLfloat, GLfloat)
  } deriving (Show, Eq)

insersectAABB :: AABB -> AABB -> Bool
insersectAABB (AABB aMin aMax) (AABB bMin bMax) =
  (aMinX <= bMaxX && aMaxX >= bMinX) &&
  (aMinY <= bMaxY && aMaxY >= bMinY) &&
  (aMinZ <= bMaxZ && aMaxZ >= bMinZ)
  where
    (aMinX, aMinY, aMinZ) = aMin
    (bMinX, bMinY, bMinZ) = bMin
    (aMaxX, aMaxY, aMaxZ) = aMax
    (bMaxX, bMaxY, bMaxZ) = bMax

--translateAABB :: V3 GLfloat -> AABB -> AABB

chunk :: Int -> [a] -> [(a, a, a)]
chunk _ [] = []
chunk n ls = (a, b, c) : chunk n lt
  where ([a, b, c], lt) = splitAt n ls

compElems :: Ord a => (a -> a -> a) -> (a, a, a) -> (a, a, a) -> (a, a, a)
compElems f (a, b, c) (x, y, z) = (f a x, f b y, f c z)

compCoords :: (Ord a) => (a, a, a) -> ((a, a, a), (a, a, a)) -> ((a, a, a), (a, a, a))
compCoords newCoords (oldMin, oldMax) = (compElems min oldMin newCoords, compElems max oldMax newCoords)

createAABB :: [GLfloat] -> AABB
createAABB vs = AABB aMax aMin
  where (aMax, aMin) = foldr compCoords ((0, 0, 0), (0, 0, 0)) (chunk 3 vs)
