module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gs = return gs { time = time gs - secs}

input :: Event -> GameState -> IO GameState
input (EventKey (Char c) Down _ _) gs | c == 'a' = return (movePlayerLeft gs)
                                        | c == 'd' = return (movePlayerRight gs)
                                        | c == 'w' = return (movePlayerUp gs)
                                        | c == 's' = return (movePlayerDown gs)
                                        | c == ' ' = return (shoot gs)
                                        | otherwise = return gs
input (EventKey (SpecialKey KeyLeft) Down _ _) gs = return (movePlayerLeft gs)
input (EventKey (SpecialKey KeyRight) Down _ _) gs = return (movePlayerRight gs)
input (EventKey (SpecialKey KeyUp) Down _ _) gs = return (movePlayerUp gs)
input (EventKey (SpecialKey KeyDown) Down _ _) gs = return (movePlayerDown gs)
input _ gs = return gs

movePlayerLeft :: GameState -> GameState
movePlayerLeft gs = undefined

movePlayerRight :: GameState -> GameState
movePlayerRight gs = undefined

movePlayerUp :: GameState -> GameState
movePlayerUp gs = undefined

movePlayerDown :: GameState -> GameState
movePlayerDown gs = undefined

shoot :: GameState -> GameState
shoot gs = undefined