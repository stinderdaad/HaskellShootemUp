module View where

import Graphics.Gloss
import Model


view :: GameState -> IO Picture
-- view gs = return (pictures ([objectToPicture (PlayerObject (player gs))]
--                             ++ objectsToPictures (objects gs)
--                             ++ [timerToPicture gs]))
view gs = do
    putStrLn $ "Rendering with GameState: " ++ show gs
    return (pictures ([objectToPicture (PlayerObject (player gs))]
                    ++ objectsToPictures (objects gs)
                    ++ [timerToPicture gs]))

objectsToPictures :: [Object] -> [Picture]
objectsToPictures [] = []
objectsToPictures xs = map objectToPicture xs

objectToPicture :: Object -> Picture
objectToPicture (PlayerObject player) = uncurry translate (positionToTuple(playerPosition player)) (color white (circleSolid 10))
objectToPicture (EnemyObject enemy) = uncurry translate (positionToTuple(enemyPosition enemy)) (color red (circleSolid 10))
objectToPicture (BulletObject bullet) = uncurry translate (positionToTuple(bulletPosition bullet)) (color yellow (circleSolid 5))

timerToPicture :: GameState -> Picture
timerToPicture gs = translate 0 300 (scale 0.2 0.2 pic)
    where pic = text (show (floorFloat (time gs)))

positionToTuple :: Position -> (Float, Float)
positionToTuple (Point x y) = (x, y)

-- This helper function rounds down a Float to the nearest whole number.
floorFloat :: Float -> Int
floorFloat flt = fromIntegral (floor flt)
