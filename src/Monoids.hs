{-# LANGUAGE RecordWildCards #-}

module Monoids where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import Body

type Timestep = Float

data Game = Game {
             pause :: Bool,
             showHelp :: Bool,
             ship :: Ship,
             obstacles :: [Body],
             screenSize :: (Float, Float)
             } deriving Show

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
initShip = Ship initBody initSteer

initGame size = Game False False initShip [] size

speedlimit = 30

radiants d = (d * atan 1) / 45
degrees  r = r * 45 / atan 1

angle :: Vector -> Float
angle (vx, vy) = degrees $ atan2 vy vx

magnitude :: Vector -> Float
magnitude (vx, vy) = sqrt (vx * vx + vy * vy)

updateShip :: (Float, Float) -> Timestep -> Ship -> Ship
updateShip dim dt ship@Ship{..} = trace (show ship) $ steerShip (ship {body = warp dim . move $ body})

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

at :: Point -> Picture -> Picture
at (x, y) = translate x y

drawShip :: Ship -> Picture
drawShip (Ship (Body _ p o _ _) _) = at p $ 
                      color green $ 
                      rotate (negate o + 90) $ 
                      polygon [(0, 15), (10, -15), (0, -5), (-10, -15)]

drawText size = scale size size . text

drawPause True = pictures [box, msg] 
    where box = color orange $ rectangleSolid w h
          msg = translate (-0.2 * w) (-0.2 * h) $ drawText 0.2 "Paused"
          (w, h) = (250, 50)
drawPause _ = blank

render :: Game -> Picture
render game@Game{..} = pictures [drawShip ship, drawPause pause]

update :: Timestep -> Game -> Game
update dt g@Game{..} = if pause 
                        then g
                        else g {ship = updateShip screenSize dt ship', obstacles = ob}
    where ship' = ship {body = sb}
          (sb:ob) = collisions (body ship : obstacles)

hitThrust x ship@Ship{..} = ship {steer = steer {cmdThrust = x}}
hitLeft x ship@Ship{..} = ship {steer = steer {cmdLeft = x}}
hitRight x ship@Ship{..} = ship {steer = steer {cmdRight = x}}

handle :: Event -> Game -> Game
handle (EventKey (SpecialKey KeyUp     ) k    _ _) g@Game{..} = g {ship = hitThrust (k == Down) ship}
handle (EventKey (SpecialKey KeyLeft   ) k    _ _) g@Game{..} = g {ship = hitLeft (k == Down) ship}
handle (EventKey (SpecialKey KeyRight  ) k    _ _) g@Game{..} = g {ship = hitRight (k == Down) ship}
handle (EventKey (SpecialKey KeyDown   ) Down _ _) g@Game{..} = g {ship = breakingFlip ship}
handle (EventKey (SpecialKey KeySpace  ) Down _ _) g@Game{..} = g {pause = not pause}
handle _ g = g

monoids = do
    (w, h) <- getScreenSize
    let game = initGame (fromIntegral w, fromIntegral h)
        fps  = 60
        bg   = black
    play FullScreen bg fps game render handle update
