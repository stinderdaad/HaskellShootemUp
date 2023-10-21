module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = return (pictures (objectsToPictures (objects gs) ++ [timerToPicture gs]))

objectsToPictures :: [Object] -> [Picture]
objectsToPictures [] = []
objectsToPictures xs = map objectToPicture xs

objectToPicture :: Object -> Picture
objectToPicture (PlayerObject player) = uncurry translate (positionToTuple(playerPosition player)) (color white (circleSolid 10))
objectToPicture (EnemyObject enemy) = uncurry translate (positionToTuple(enemyPosition enemy)) (color red (circleSolid 10))
objectToPicture (BulletObject bullet) = uncurry translate (positionToTuple(bulletPosition bullet)) (color blue (circleSolid 5))

positionToTuple :: Position -> (Float, Float)
positionToTuple (Point x y) = (x, y)

timerToPicture :: GameState -> Picture
timerToPicture gs = translate (-100) 0 (scale 0.2 0.2 (text (show (time gs))))