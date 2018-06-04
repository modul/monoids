{-# LANGUAGE RecordWildCards #-}
module Draw where

import Graphics.Gloss

import Body
import Ship
import Shapes


at :: Point -> Picture -> Picture
at (x, y) = translate x y

drawShip :: Ship -> Picture
drawShip Ship{..} = drawBody body

drawBody Body{..} = at pos $
                     color (if collisionWarning then orange else colour) $
                     rotate (negate ori + 90) $ 
                     pictures [circle (mass/2), scale mass mass $ polygon shape]

drawText size = scale size size . text

drawPause True = pictures [box, msg] 
    where box = color orange $ rectangleSolid w h
          msg = translate (-0.2 * w) (-0.2 * h) $ drawText 0.2 "Paused"
          (w, h) = (250, 50)
drawPause _ = blank

