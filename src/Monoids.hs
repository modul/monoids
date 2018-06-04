{-# LANGUAGE RecordWildCards #-}
module Monoids where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import Data.Maybe

import System.Random

import Body
import Ship
import Asteroid
import Draw
import Shapes
import Util

type Timestep = Float

data Game = Game {
             pause :: Bool,
             showHelp :: Bool,
             ship :: Ship,
             obstacles :: [Body],
             screenSize :: (Float, Float)
             } deriving Show

initGame size astr = Game False False initShip astr size

render :: Game -> Picture
render game@Game{..} = pictures $ [drawShip ship, drawPause pause] ++ map drawBody obstacles 

update :: Timestep -> Game -> Game
update _ g@Game{..} = if pause 
                        then g
                        else g {ship = updateShip screenSize ship', 
                                obstacles = updateAsteroids screenSize ob}
    where ship' = ship {body = sb}
          (sb:ob) = collisions (body ship : obstacles)

hitThrust x ship@Ship{..} = ship {steer = steer {cmdThrust = x}}
hitLeft x ship@Ship{..} = ship {steer = steer {cmdLeft = x}}
hitRight x ship@Ship{..} = ship {steer = steer {cmdRight = x}}

fire :: Ship -> (Ship, Maybe Body)
fire ship@Ship{..} = trace (show bullet) $ (ship, Just bullet)
    where bullet = initBody {velo = v', pos = vpos, ori = o, mass = 5, 
                             shape = [(-0.5, 0.5), (0.5, 0.5), (0.5, -0.5), (-0.5, -0.5)],
                             colour = red}
          (v, p, o, r) = (velo body, pos body, radiants (ori body), mass body / 2)
          vpos = p + (1.5 * r * cos o, 1.5 * r * sin o)
          vmag = 10
          varg = o
          v' = (vmag * cos varg, vmag * sin varg) + v -- that would be correct but less fun, maybe?


handle :: Event -> Game -> Game
handle (EventKey (SpecialKey KeyUp     ) k    _ _) g@Game{..} = g {ship = hitThrust (k == Down) ship}
handle (EventKey (SpecialKey KeyLeft   ) k    _ _) g@Game{..} = g {ship = hitLeft (k == Down) ship}
handle (EventKey (SpecialKey KeyRight  ) k    _ _) g@Game{..} = g {ship = hitRight (k == Down) ship}
handle (EventKey (SpecialKey KeyDown   ) Down _ _) g@Game{..} = g {ship = breakingFlip ship}
handle (EventKey (SpecialKey KeySpace  ) Down _ _) g@Game{..} = g {pause = not pause}
handle (EventKey (SpecialKey KeyShiftR ) Down _ _) g@Game{..} = g {ship = ship', obstacles = obst'}
    where (ship', projectiles) = fire ship
          obst' = obstacles ++ maybeToList projectiles
handle _ g = g

monoids = do
    (w, h) <- getScreenSize
    gen <- newStdGen
    let game = initGame (fromIntegral w, fromIntegral h) astr
        fps  = 60
        bg   = black
        siz  = randomRs (50, 250) gen
        pos  = randomRs (-h, h) gen
        astr = asteroids 7 siz pos
    print game
    play FullScreen bg fps game render handle update
