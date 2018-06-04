module Util (
                module Debug.Trace,
                module Graphics.Gloss,
                module Util
            ) where

import Debug.Trace
import Graphics.Gloss

type Dimension = Point

angle :: Vector -> Float
angle (vx, vy) = degrees $ atan2 vy vx

magnitude :: Vector -> Float
magnitude (vx, vy) = sqrt (vx * vx + vy * vy)

radiants d = (d * atan 1) / 45
degrees  r = r * 45 / atan 1

