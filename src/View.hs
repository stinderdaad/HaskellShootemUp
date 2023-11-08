module View where

import Graphics.Gloss
import Model

type Sprite = Picture

-- # Impure # --

view :: GameState -> IO Picture
view gs = do
--     putStrLn $ "Rendering with GameState: " ++ show gs
    playerSprite <- loadPlayerSprite
    basicEnemySprite <- loadBasicEnemySprite
    bulletSprite <- loadBulletSprite
    bossSprite <- loadBossSprite
    return (pictures (
            [playerToPicture (player gs) playerSprite] ++
            bulletsToPictures (bullets gs) bulletSprite ++
            enemiesToPictures (enemies gs) (basicEnemySprite, basicEnemySprite, bossSprite) ++
            [timerToPicture gs] ++
            [scoreToPicture gs] ++
            [livesToPicture gs] ++
            buttonsToPictures (buttons gs)
            ))


-- # Pictures # --

-- objectsToPictures :: [Object] -> (Picture, Picture, Picture, Picture) -> [Picture]
-- objectsToPictures [] _ = []
-- objectsToPictures (PlayerObject player:xs) sprites@(playerSprite, _, _, _) =
--     Color green (drawBox (PlayerObject player)) : -- hitbox
--     objectToPicture (PlayerObject player) (rotate 90 (scale 2 2 playerSprite)) :
--     objectsToPictures xs sprites
-- objectsToPictures (EnemyObject enemy:xs) sprites@(_, basicEnemySprite, _, _) =
--     Color red (drawBox (EnemyObject enemy)) : -- hitbox
--     objectToPicture (EnemyObject enemy) (rotate 270 (scale 2 2 basicEnemySprite)) :
--     objectsToPictures xs sprites
-- objectsToPictures (BulletObject bullet:xs) sprites@(_, _, bulletSprite, _) =
--     Color black (drawBox (BulletObject bullet)) :  -- hitbox
--     objectToPicture (BulletObject bullet) (rotate 90 (scale 1.5 1.5 bulletSprite)) :
--     objectsToPictures xs sprites
-- objectsToPictures (BossObject boss:xs) sprites@(_, _, _, bossSprite) =
--     Color red (drawBox (BossObject boss)) : -- hitbox
--     objectToPicture (BossObject boss) (rotate 270 (scale 8 8 bossSprite)) :
--     objectsToPictures xs sprites
-- objectsToPictures _ _ = []

-- objectToPicture :: Object -> Picture -> Picture
-- objectToPicture obj = uncurry translate (positionToTuple (objectPosition obj))

-- buttonsToPictures :: [Button] -> [Picture]
-- buttonsToPictures [] = []
-- buttonsToPictures (x:xs) = Color white (buttonToPicture (ButtonObject x)) :
--                            buttonTextToPicture (ButtonObject x) :
--                            buttonsToPictures xs

-- buttonToPicture :: Object -> Picture
-- buttonToPicture (ButtonObject button) = drawBox (ButtonObject button)
-- buttonToPicture _ = Blank

-- buttonTextToPicture :: Object -> Picture
-- buttonTextToPicture (ButtonObject button) = translate (x - 75) y pic
--     where (x, y) = positionToTuple (objectPosition (ButtonObject button))
--           pic = scale 0.2 0.2 (text (buttonText button))

hitboxToPicture :: GameObject a => a -> Picture
hitboxToPicture obj = Color red (drawBox obj)

playerToPicture :: Player -> Sprite -> Picture
playerToPicture player sprite =
    uncurry translate (position player) (rotate 90 (scale 2 2 sprite))

enemiesToPictures :: [Enemy] -> (Sprite, Sprite, Sprite) -> [Picture]
enemiesToPictures [] _ = [Blank]
enemiesToPictures (enemy:enemies) (basicEnemySprite, toughEnemySprite, bossSprite)
    | enemyType enemy == BasicEnemy =
        basicEnemyToPicture enemy basicEnemySprite :
        enemiesToPictures enemies (basicEnemySprite, toughEnemySprite, bossSprite)
    | enemyType enemy == ToughEnemy =
        toughEnemyToPicture enemy toughEnemySprite :
        enemiesToPictures enemies (basicEnemySprite, toughEnemySprite, bossSprite)
    | enemyType enemy == BossEnemy =
        bossToPicture enemy bossSprite :
        enemiesToPictures enemies (basicEnemySprite, toughEnemySprite, bossSprite)

basicEnemyToPicture :: Enemy -> Sprite -> Picture
basicEnemyToPicture enemy sprite =
    uncurry translate (position enemy) (rotate 270 (scale 2 2 sprite))

toughEnemyToPicture :: Enemy -> Sprite -> Picture
toughEnemyToPicture enemy sprite =
    uncurry translate (position enemy) (rotate 270 (scale 4 4 sprite))

bossToPicture :: Enemy -> Sprite -> Picture
bossToPicture boss sprite =
    uncurry translate (position boss) (rotate 270 (scale 8 8 sprite))

bulletsToPictures :: [Bullet] -> Sprite -> [Picture]
bulletsToPictures bullets sprite = map (`bulletToPicture` sprite) bullets

bulletToPicture :: Bullet -> Sprite -> Picture
bulletToPicture bullet sprite =
    uncurry translate (position bullet) (rotate 90 (scale 1.5 1.5 sprite))

buttonsToPictures :: [Button] -> [Picture]
buttonsToPictures = map buttonToPicture

buttonToPicture :: Button -> Picture
buttonToPicture button =
    pictures (color red (drawBox button) : [buttonTextToPicture button])

buttonTextToPicture :: Button -> Picture
buttonTextToPicture button = translate (x - 75) y pic
    where (x, y) = position button
          pic = scale 0.2 0.2 (text (buttonText button))

buttonText :: Button -> String
buttonText (Button _ _ Start) = "Start Game"
buttonText (Button _ _ Quit) = "Quit Game"
buttonText (Button _ _ ToHighScore) = "High Scores"
buttonText (Button _ _ Retry) = "Retry"
buttonText (Button _ _ Resume) = "Resume"
buttonText (Button _ _ ToMainMenu) = "Main Menu"
-- buttonText _ = "Button"

-- handy for hitboxes and buttons
drawBox :: GameObject a => a -> Picture
drawBox obj = polygon [nW, nE, sE, sW]
    where (nW, nE, sE, sW) = objectCorners obj

timerToPicture :: GameState -> Picture
timerToPicture gs
    | menu gs == GameOverMenu = translate (-50) 300 (scale 0.2 0.2 gameOver)
    | menu gs == PauseMenu = translate (-50) 300 (scale 0.2 0.2 pause)
    | menu gs == VictoryMenu = translate (-50) 300 (scale 0.2 0.2 victory)
    | time gs == -10 = translate (-50) 300 (scale 0.2 0.2 boss)
    | otherwise = translate (-100) 300 (scale 0.2 0.2 pic)
    where pic = text ("Time until boss: " ++ show (floorFloat (time gs)))
          boss = text "Boss Battle!"
          pause = text "Paused"
          gameOver = text "Game Over"
          victory = text "Victory!"

scoreToPicture :: GameState -> Picture
scoreToPicture gs = translate (-700) 300 (scale 0.2 0.2 pic)
    where pic = text ("Score: " ++ show (score gs))

livesToPicture :: GameState -> Picture
livesToPicture gs = translate 500 300 (scale 0.2 0.2 pic)
    where pic = text ("Lives left: " ++ show (playerHealth (player gs)))


-- # Sprites # --

loadPlayerSprite :: IO Picture
loadPlayerSprite = loadBMP "./sprites/Ships/PlayerShip.bmp"

loadBasicEnemySprite :: IO Picture
loadBasicEnemySprite = loadBMP "./sprites/Ships/EnemyBasic.bmp"

loadBulletSprite :: IO Picture
loadBulletSprite = loadBMP "./sprites/Misc/Bullet.bmp"

loadBossSprite :: IO Picture
loadBossSprite = loadBMP "./sprites/Ships/EnemyKamikaze.bmp"


-- # Helper Functions # --

-- positionToTuple :: Position -> (Float, Float)
-- positionToTuple (Point x y) = (x, y)

-- This helper function rounds down a Float to the nearest whole number.
floorFloat :: Float -> Int
floorFloat flt = fromIntegral (floor flt)
