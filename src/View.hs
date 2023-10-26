module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = do
--     putStrLn $ "Rendering with GameState: " ++ show gs
    playerSprite <- loadPlayerSprite
    basicEnemySprite <- loadBasicEnemySprite
    bulletSprite <- loadBulletSprite
    bossSprite <- loadBossSprite
    return (pictures ( objectsToPictures (allObjects gs) (playerSprite, basicEnemySprite, bulletSprite, bossSprite)
                    ++ [timerToPicture gs]))

objectsToPictures :: [Object] -> (Picture, Picture, Picture, Picture) -> [Picture]
objectsToPictures [] _ = []
objectsToPictures (PlayerObject player:xs) sprites@(playerSprite, _, _, _) =
    drawHitBox (PlayerObject player) :
    objectToPicture (PlayerObject player) (rotate 90 (scale 2 2 playerSprite)) :
    objectsToPictures xs sprites
objectsToPictures (EnemyObject enemy:xs) sprites@(_, basicEnemySprite, _, _) =
    drawHitBox (EnemyObject enemy) :
    objectToPicture (EnemyObject enemy) (rotate 270 (scale 2 2 basicEnemySprite)) :
    objectsToPictures xs sprites
objectsToPictures (BulletObject bullet:xs) sprites@(_, _, bulletSprite, _) =
    drawHitBox (BulletObject bullet) :
    objectToPicture (BulletObject bullet) (rotate 90 (scale 1.5 1.5 bulletSprite)) :
    objectsToPictures xs sprites
objectsToPictures (BossObject boss:xs) sprites@(_, _, _, bossSprite) =
    drawHitBox (BossObject boss) :
    objectToPicture (BossObject boss) (rotate 270 (scale 8 8 bossSprite)) :
    objectsToPictures xs sprites
objectsToPictures _ _ = []

objectToPicture :: Object -> Picture -> Picture
objectToPicture obj = uncurry translate (positionToTuple (objectPosition obj))

drawHitBox :: Object -> Picture
drawHitBox obj = polygon [positionToTuple (objectCornerNW obj),
                          positionToTuple (objectCornerNE obj),
                          positionToTuple (objectCornerSE obj),
                          positionToTuple (objectCornerSW obj)]

timerToPicture :: GameState -> Picture
timerToPicture gs
    | menu gs == PauseMenu = translate 0 300 (scale 0.2 0.2 pause)
    | time gs == -10 = translate (-50) 300 (scale 0.2 0.2 boss)
    | otherwise = translate 0 300 (scale 0.2 0.2 pic)
    where pic = text (show (floorFloat (time gs)))
          boss = text "Boss Battle!"
          pause = text "Paused"


-- Sprites

loadPlayerSprite :: IO Picture
loadPlayerSprite = loadBMP "./sprites/Ships/PlayerShip.bmp"

loadBasicEnemySprite :: IO Picture
loadBasicEnemySprite = loadBMP "./sprites/Ships/EnemyBasic.bmp"

loadBulletSprite :: IO Picture
loadBulletSprite = loadBMP "./sprites/Misc/Bullet.bmp"

loadBossSprite :: IO Picture
loadBossSprite = loadBMP "./sprites/Ships/EnemyKamikaze.bmp"

-- Helper Functions

positionToTuple :: Position -> (Float, Float)
positionToTuple (Point x y) = (x, y)

-- This helper function rounds down a Float to the nearest whole number.
floorFloat :: Float -> Int
floorFloat flt = fromIntegral (floor flt)
