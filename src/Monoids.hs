{-# LANGUAGE RecordWildCards #-}

module Monoids where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import Body
import Shapes

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
initShip = Ship (initBody {shape = shipShape}) initSteer

initGame size = Game False False initShip asteroids size

asteroids :: [Body]
asteroids = zipWith3 mkAsteroid masss pos speed 
    where masss = [100, 75, 40, 200]
          speed = [(x / 100, y / 100) | (x, y) <- pos]
          pos   = [400, -200, (600, -30), (-123, 123)] :: [Point]

speedlimit = 30

radiants d = (d * atan 1) / 45
degrees  r = r * 45 / atan 1

angle :: Vector -> Float
angle (vx, vy) = degrees $ atan2 vy vx

magnitude :: Vector -> Float
magnitude (vx, vy) = sqrt (vx * vx + vy * vy)

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

mkAsteroid s p v = Body v p s s asteroidShape False

at :: Point -> Picture -> Picture
at (x, y) = translate x y

drawShip :: Ship -> Picture
drawShip (Ship (Body _ p o _ s c) _) = at p $ 
                      color (if c then orange else green) $ 
                      rotate (negate o + 90) $ 
                      polygon s

drawBody Body{..} = at pos $
                     color (if collisionWarning then orange else white) $
                     rotate (negate ori + 90) $ 
                     scale mass mass $
                     polygon shape

drawText size = scale size size . text

drawPause True = pictures [box, msg] 
    where box = color orange $ rectangleSolid w h
          msg = translate (-0.2 * w) (-0.2 * h) $ drawText 0.2 "Paused"
          (w, h) = (250, 50)
drawPause _ = blank

render :: Game -> Picture
render game@Game{..} = pictures $ [drawShip ship, drawPause pause] ++ map drawBody obstacles 

updateAsteroids :: Dimension -> [Body] -> [Body]
updateAsteroids dim bs = map (warp dim . move) bs

update :: Timestep -> Game -> Game
update _ g@Game{..} = if pause 
                        then g
                        else g {ship = updateShip screenSize ship', obstacles = updateAsteroids screenSize ob}
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
    print game
    play FullScreen bg fps game render handle update
