module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gs = return gs { objects = updateObjects (objects gs),
                           time = time gs - secs
                        }

-- input :: Event -> GameState -> IO GameState
-- input (EventKey (Char c) Down _ _) gs | c == 'a' = return (movePlayerLeft gs)
--                                         | c == 'd' = return (movePlayerRight gs)
--                                         | c == 'w' = return (movePlayerUp gs)
--                                         | c == 's' = return (movePlayerDown gs)
--                                         | c == ' ' = return (shoot gs)
--                                         | otherwise = return gs
-- input (EventKey (SpecialKey KeyLeft) Down _ _) gs = return (movePlayerLeft gs)
-- input (EventKey (SpecialKey KeyRight) Down _ _) gs = return (movePlayerRight gs)
-- input (EventKey (SpecialKey KeyUp) Down _ _) gs = return (movePlayerUp gs)
-- input (EventKey (SpecialKey KeyDown) Down _ _) gs = return (movePlayerDown gs)
-- input _ gs = return gs

input :: Event -> GameState -> IO GameState
input event gs = do
    putStrLn $ "Received event: " ++ show event
    return $ case event of
        EventKey (Char c) Down _ _ -> handleCharKey c gs
        EventKey (SpecialKey key) Down _ _ -> handleSpecialKey key gs
        _ -> gs

handleCharKey :: Char -> GameState -> GameState
handleCharKey c gs
    | c == 'a' = movePlayerLeft gs
    | c == 'd' = movePlayerRight gs
    | c == 'w' = movePlayerUp gs
    | c == 's' = movePlayerDown gs
    | c == ' ' = shoot gs
    | otherwise = gs

handleSpecialKey :: SpecialKey -> GameState -> GameState
handleSpecialKey KeyLeft gs = movePlayerLeft gs
handleSpecialKey KeyRight gs = movePlayerRight gs
handleSpecialKey KeyUp gs = movePlayerUp gs
handleSpecialKey KeyDown gs = movePlayerDown gs
handleSpecialKey _ gs = gs
