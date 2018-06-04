{-# LANGUAGE RecordWildCards #-}
module Ship where

import Util
import Body
import Shapes

data Ship = Ship {
              body :: Body,
              steer :: Steer
            } deriving Show

data Steer = Steer {
                cmdThrust :: Bool,
                cmdLeft :: Bool,
                cmdRight :: Bool
            } deriving Show

initSteer = Steer False False False
initShip = Ship body initSteer
    where body = initBody {shape = shipShape, mass = 30, colour = green}

updateShip :: Dimension -> Ship -> Ship
updateShip dim ship@Ship{..} = trace (show ship) $ steerShip (ship {body = warp dim . move $ body})

steerShip ship@Ship{..} = ship {body = body {velo = v', ori = o'}}
    where v  = velocity + thrust inc velocity orientation
          v' = if magnitude v > speedlimit then velocity else v
          o' = orientation + deg * dir
          deg = 5.0
          inc = if cmdThrust steer then 0.1 else 0
          dir | cmdLeft steer  =  1
              | cmdRight steer = -1
              | otherwise =  0
          velocity = velo body
          orientation = ori body
            
breakingFlip ship@Ship{..} = ship {body = body {ori = angle velocity + 180}}
    where velocity = velo body
          
thrust :: Float -> Vector -> Float -> Vector
thrust inc velocity orientation = (vx, vy)
    where vy = inc * sin ag
          vx = inc * cos ag
          ag = radiants orientation


