module Main where

import Graphics.Gloss.Interface.IO.Game
import Controller
import Model
import View

fps :: Int
fps = 10

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              blue            -- Background color
              fps               -- Frames per second
              initState     -- Initial state
              undefined--view             -- View function
              undefined--input            -- Event function
              undefined--step             -- Step function