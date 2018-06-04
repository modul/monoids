{-# LANGUAGE RecordWildCards #-}
module Asteroid where

import Util
import Body
import Shapes

mkAsteroid s p v = Body v p s s asteroidShape white False

asteroids :: Int -> [Int] -> [Int] -> [Body]
asteroids count dim place = zipWith3 mkAsteroid mass pos speed 
    where speed = [(x / 1000, y / 1000) | (x, y) <- pos]
          mass = map fromIntegral dim
          pos = take count $ zip x (drop 2 x)
          x = map fromIntegral place


updateAsteroids :: Dimension -> [Body] -> [Body]
updateAsteroids dim bs = map (warp dim . move) bs

