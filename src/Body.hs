{-# LANGUAGE RecordWildCards #-}
module Body where

import Graphics.Gloss

type Dimension = Point

-- | Generic body type. 
data Body = Body {
                velo :: Vector,
                pos :: Point,
                ori :: Float,
                mass :: Float,
                collisionWarning :: Bool
            } deriving (Show, Eq)

-- | Update the position of a body.
move :: Body -> Body
move body@Body{..} = body {pos = pos + velo}

-- | Keep a body within given the dimensions of the world, by warping space around.
warp :: Dimension -> Body -> Body
warp (bx, by) body@Body{..} = body {pos = warp' pos}
    where warp' (x, y) = (x', y')
            where x' | x > r = l
                     | x < l = r
                     | otherwise = x
                  y' | y > t = b
                     | y < b = t
                     | otherwise = y
                  r = bx / 2
                  t = by / 2
                  l = -r
                  b = -t

-- | Detect a collision between two circular bodies.
doesCollide :: Body -> Body -> Bool
doesCollide a b = collidesWith b
    where collidesWith Body{..} = within p mass pos
          p = pos a

-- | Check all bodies for collisions and update them if neccessary.
collisions :: [Body] -> [Body]
collisions obj = map update obj
    where cls x = any (doesCollide x) [b | b <- obj, b /= x]
          update x = x {collisionWarning = cls x}

-- | Check if a point '(x, y)' is within or on the radius around point 'c'.
within :: Point -> Float -> Point -> Bool
within (x, y) r (cx, cy) = (x - cx)^2 + (y - cy)^2 <= r^2
