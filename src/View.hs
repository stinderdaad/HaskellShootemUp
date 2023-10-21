module View where

import Graphics.Gloss
import Model
import HelperFunctions

view :: GameState -> IO Picture
view gs = return (pictures (objectsToPictures (objects gs) ++ [timerToPicture gs]))

objectsToPictures :: [Object] -> [Picture]
objectsToPictures [] = []
objectsToPictures xs = map objectToPicture xs

objectToPicture :: Object -> Picture
objectToPicture (PlayerObject player) = uncurry translate (positionToTuple(playerPosition player)) (color white (circleSolid 10))
objectToPicture (EnemyObject enemy) = uncurry translate (positionToTuple(enemyPosition enemy)) (color red (circleSolid 10))
objectToPicture (BulletObject bullet) = uncurry translate (positionToTuple(bulletPosition bullet)) (color blue (circleSolid 5))

timerToPicture :: GameState -> Picture
timerToPicture gs = translate 0 300 (scale 0.2 0.2 pic)
    where pic = text (show (floorFloat (time gs)))