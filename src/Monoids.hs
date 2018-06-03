{-# LANGUAGE RecordWildCards #-}

module Monoids where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

type Timestep = Float

data Game = Game {
             run :: Bool,
             showHelp :: Bool,
             ship :: Ship,
             obstacles :: [Object],
             screenSize :: (Float, Float)
             } deriving Show

data Object = Object {size :: Float} deriving Show             

data Ship = Ship {
                velocity :: Vector,
                position :: Point,
                orientation :: Float,
                cmdThrust :: Bool,
                cmdLeft :: Bool,
                cmdRight :: Bool
            } deriving Show

initShip = Ship (0.0, 0.0) (0.0, 0.0) 0.0 False False False

initGame size = Game True False initShip [] size

speedlimit = 30

radiants d = (d * atan 1) / 45
degrees  r = r * 45 / atan 1

warp (x, y) (bx, by) = (x', y')
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

angle :: Vector -> Float
angle (vx, vy) = degrees $ atan2 vy vx

magnitude :: Vector -> Float
magnitude (vx, vy) = sqrt (vx * vx + vy * vy)

updateShip :: (Float, Float) -> Timestep -> Ship -> Ship
updateShip size dt ship = trace (show ship) $ moveShip size . steerShip $ ship

moveShip size ship@Ship{..} = ship {position = warp (position + velocity) size}

steerShip ship@Ship{..} = ship {velocity = v', orientation = o'}
    where v  = velocity + thrust inc velocity orientation
          v' = if magnitude v > speedlimit then velocity else v
          o' = orientation + deg * dir
          deg = 5.0
          inc = if cmdThrust then 0.1 else 0
          dir | cmdLeft   =  1
              | cmdRight  = -1
              | otherwise =  0
            
breakingFlip ship@Ship{..} = ship {orientation = angle velocity + 180}
    where theta = angle velocity
          
thrust :: Float -> Vector -> Float -> Vector
thrust inc velocity orientation = (vx, vy)
    where vy = inc * sin ag
          vx = inc * cos ag
          ag = radiants orientation

at :: Point -> Picture -> Picture
at (x, y) = translate x y

drawShip :: Ship -> Picture
drawShip Ship{..} = at position $ 
                      color green $ 
                      rotate (negate orientation + 90) $ 
                      polygon [(0, 15), (10, -15), (0, -5), (-10, -15)]

render :: Game -> Picture
render game@Game{..} = pictures [drawShip ship]

update :: Timestep -> Game -> Game
update dt g@Game{..} = g {ship = updateShip screenSize dt ship}

handle :: Event -> Game -> Game
handle (EventKey (SpecialKey KeyUp     ) k    _ _) g@Game{..} = g {ship = ship {cmdThrust = k == Down}}
handle (EventKey (SpecialKey KeyLeft   ) k    _ _) g@Game{..} = g {ship = ship {cmdLeft = k == Down}}
handle (EventKey (SpecialKey KeyRight  ) k    _ _) g@Game{..} = g {ship = ship {cmdRight = k == Down}}
handle (EventKey (SpecialKey KeyDown   ) Down _ _) g@Game{..} = g {ship = breakingFlip ship}
handle _ g = g

monoids = do
    (w, h) <- getScreenSize
    let game = initGame (fromIntegral w, fromIntegral h)
        fps  = 60
        bg   = black
    play FullScreen bg fps game render handle update
