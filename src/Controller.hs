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
        EventKey (Char c) Down _ _ -> handleCharKeyDown c gs
        EventKey (Char c) Up _ _ -> handleCharKeyUp c gs
        EventKey (SpecialKey key) Down _ _ -> handleSpecialKeyDown key gs
        EventKey (SpecialKey key) Up _ _ -> handleSpecialKeyUp key gs
        _ -> gs

handleCharKeyDown :: Char -> GameState -> GameState
handleCharKeyDown c gs
    | c == 'a' = movePlayerLeft gs
    | c == 'd' = movePlayerRight gs
    | c == 'w' = movePlayerUp gs
    | c == 's' = movePlayerDown gs
    | c == ' ' = shoot gs
    | otherwise = gs

handleCharKeyUp :: Char -> GameState -> GameState
handleCharKeyUp c gs
    | c == 'a' = movePlayerRight gs
    | c == 'd' = movePlayerLeft gs
    | c == 'w' = movePlayerDown gs
    | c == 's' = movePlayerUp gs
    | otherwise = gs

handleSpecialKeyDown :: SpecialKey -> GameState -> GameState
handleSpecialKeyDown KeyLeft gs = movePlayerLeft gs
handleSpecialKeyDown KeyRight gs = movePlayerRight gs
handleSpecialKeyDown KeyUp gs = movePlayerUp gs
handleSpecialKeyDown KeyDown gs = movePlayerDown gs
handleSpecialKeyDown _ gs = gs

handleSpecialKeyUp :: SpecialKey -> GameState -> GameState
handleSpecialKeyUp KeyLeft gs = movePlayerRight gs
handleSpecialKeyUp KeyRight gs = movePlayerLeft gs
handleSpecialKeyUp KeyUp gs = movePlayerDown gs
handleSpecialKeyUp KeyDown gs = movePlayerUp gs
handleSpecialKeyUp _ gs = gs
