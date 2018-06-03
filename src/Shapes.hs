module Shapes where

import Graphics.Gloss

-- All shapes need to be within [(0.5, 0.5), (-0.5, -0.5)], i.e. size 1x1, centered around (0, 0).

asteroidShape :: Path
asteroidShape = [
                 ( 0.00,  0.50), ( 0.35,  0.50), 
                 ( 0.50,  0.25), ( 0.50, -0.25), 
                 ( 0.25, -0.50), ( 0.00, -0.50),
                 (-0.25, -0.50), (-0.40, -0.25),
                 (-0.50,  0.25), (-0.25,  0.40)
                ]

shipShape :: Path
shipShape = [
             (0,  0.50), ( 0.33, -0.50), 
             (0, -0.17), (-0.33, -0.50)
            ]

