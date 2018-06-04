{-# LANGUAGE RecordWildCards #-}
module Monoids where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import System.Random

import Body
import Ship
import Asteroid
import Draw
import Shapes

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

handle :: Event -> Game -> Game
handle (EventKey (SpecialKey KeyUp     ) k    _ _) g@Game{..} = g {ship = hitThrust (k == Down) ship}
handle (EventKey (SpecialKey KeyLeft   ) k    _ _) g@Game{..} = g {ship = hitLeft (k == Down) ship}
handle (EventKey (SpecialKey KeyRight  ) k    _ _) g@Game{..} = g {ship = hitRight (k == Down) ship}
handle (EventKey (SpecialKey KeyDown   ) Down _ _) g@Game{..} = g {ship = breakingFlip ship}
handle (EventKey (SpecialKey KeySpace  ) Down _ _) g@Game{..} = g {pause = not pause}
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
