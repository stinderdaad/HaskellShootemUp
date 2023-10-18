module Controller where
import Model
import Graphics.Gloss.Interface.IO.Game

-- if player presses 'a' or 'd', move player left or right
-- movePlayer :: Player -> Player
-- movePlayer player = player { playerPosition = playerPosition player }

inputKey :: Event -> GameState -> IO GameState
inputKey (EventKey (Char c) Down _ _) gs | c == 'a' = return (movePlayerLeft gs)
                                        | c == 'd' = return (movePlayerRight gs)
                                        | c == 'w' = return (movePlayerUp gs)
                                        | c == 's' = return (movePlayerDown gs)
                                        | c == ' ' = return (shoot gs)
                                        | otherwise = return gs
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gs = return (movePlayerLeft gs)
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gs = return (movePlayerRight gs)
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gs = return (movePlayerUp gs)
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gs = return (movePlayerDown gs)
inputKey _ gs = return gs

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