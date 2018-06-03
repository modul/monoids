module Shapes where

import Graphics.Gloss

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
             (0, 15), ( 10, -15), 
             (0, -5), (-10, -15)
            ]

