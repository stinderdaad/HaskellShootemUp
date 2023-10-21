module Main where

import Graphics.Gloss.Interface.IO.Game
import Controller
import Model
import View

fps :: Int
fps = 10

main :: IO ()
main = playIO (InWindow "Counter" (1600, 800) (0, 0)) -- Or FullScreen
              blue                                   -- Background color
              fps                                    -- Frames per second
              initState                              -- Initial state
              view                                   -- View function
              input                                  -- Event function
              step                                   -- Step function