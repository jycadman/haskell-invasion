module Main where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Alien Invasion" (200, 200) (10, 10)) white (Circle 80)
