module View where

import Graphics.Gloss
import Model
import GHC.IO.Handle.FD
import GHC.IO.IOMode
import GHC.IO.Handle


-- # Types # --

type Animation = [Sprite]

type Sprite = Picture


-- # Impure # --

view :: GameState -> IO Picture
view gs = do
--     putStrLn $ "Rendering with GameState: " ++ show gs
    playerSprite <- loadPlayerSprite
    basicEnemySprite <- loadBasicEnemySprite
    toughEnemySprite <- loadToughEnemySprite
    smartEnemySprite <- loadSmartEnemySprite
    bulletSprite <- loadBulletSprite
    bossSprite <- loadBossSprite
    dualBossSprite <- loadDualBossSprite
    explosionSprites <- loadExplosionFrames
    highScores <- loadHighScores
    return (pictures (
            --allHitboxesToPictures gs : -- only for debugging
            [playerToPicture (player gs) playerSprite] ++
            enemiesToPictures (enemies gs) (basicEnemySprite,
                                            toughEnemySprite,
                                            smartEnemySprite,
                                            bossSprite,
                                            dualBossSprite) ++
            bulletsToPictures (bullets gs) bulletSprite ++
            animationStatesToPictures (animations gs) explosionSprites ++
            [timerToPicture gs] ++
            [scoreToPicture gs] ++
            [livesToPicture gs] ++
            [highScoresToPicture gs highScores] ++
            buttonsToPictures (buttons gs)
            ))


-- # Load sprites/data # --

loadPlayerSprite :: IO Picture
loadPlayerSprite = loadBMP "./sprites/Ships/PlayerShip.bmp"

loadBasicEnemySprite :: IO Picture
loadBasicEnemySprite = loadBMP "./sprites/Ships/EnemyBasic.bmp"

loadToughEnemySprite :: IO Picture
loadToughEnemySprite = loadBMP "./sprites/Ships/EnemyTough.bmp"

loadSmartEnemySprite :: IO Picture
loadSmartEnemySprite = loadBMP "./sprites/Ships/EnemySmart.bmp"

loadBulletSprite :: IO Picture
loadBulletSprite = loadBMP "./sprites/Misc/Bullet.bmp"

loadBossSprite :: IO Picture
loadBossSprite = loadBMP "./sprites/Ships/BasicBoss.bmp"

loadDualBossSprite :: IO Picture
loadDualBossSprite = loadBMP "./sprites/Ships/DualBoss.bmp"

loadHighScores :: IO String
loadHighScores = readFile "data/HighScores.txt"

loadExplosionFrames :: IO [Picture]
loadExplosionFrames = do
    let path = "./sprites/Misc/explosion_frame"
    let ext = ".bmp"
    let frames = [1, 2, 3]
    mapM (\n -> loadBMP (path ++ show n ++ ext)) frames


-- # Pure # --
-- # Pictures # --

animationStatesToPictures :: [AnimationState] -> Animation -> [Picture]
animationStatesToPictures states sprites = map (animationStateToPicture sprites) states

animationStateToPicture :: Animation -> AnimationState -> Picture
animationStateToPicture sprites state =
    uncurry translate (animationPosition state) (scale 2 2 (sprites !! (animationCurrentSprite state - 1)))

allHitboxesToPictures :: GameState -> Picture
allHitboxesToPictures gs = pictures (hitboxToPicture (player gs) :
                                     map hitboxToPicture (enemies gs) ++
                                     map hitboxToPicture (bullets gs) ++
                                     map hitboxToPicture (walls gs))

hitboxToPicture :: GameObject a => a -> Picture
hitboxToPicture obj = Color red (drawBox obj)

playerToPicture :: Player -> Sprite -> Picture
playerToPicture player sprite =
    uncurry translate (position player) (rotate 90 (scale 2 2 sprite))

enemiesToPictures :: [Enemy] -> (Sprite, Sprite, Sprite, Sprite, Sprite) -> [Picture]
enemiesToPictures [] _ = [Blank]
enemiesToPictures (enemy:enemies) sprites@(basicEnemySprite, toughEnemySprite, smartEnemySprite, bossSprite, dualSprite)
    | enemyType enemy == BasicEnemy =
        basicEnemyToPicture enemy basicEnemySprite :
        enemiesToPictures enemies sprites
    | enemyType enemy == ToughEnemy =
        toughEnemyToPicture enemy (scale 1.2 1.2 toughEnemySprite) :
        enemiesToPictures enemies sprites
    | enemyType enemy == SmartEnemy =
        basicEnemyToPicture enemy (scale 1.4 1.4 smartEnemySprite) :
        enemiesToPictures enemies sprites
    | enemyType enemy == BossEnemy && height == 799 =
        bossToPicture enemy (translate 0 (-25) (scale 5 5 bossSprite)) :
        enemiesToPictures enemies sprites
    | enemyType enemy == BossEnemy && hasDualAttack enemy =
        bossToPicture enemy (translate 0 (-5) (scale 1.5 1.5 dualSprite)) :
        enemiesToPictures enemies sprites
    | enemyType enemy == BossEnemy =
        bossToPicture enemy bossSprite :
        enemiesToPictures enemies sprites
    where (width, height) = enemySize enemy

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
bulletToPicture bullet sprite
    | bulletDirection bullet == (1, 0) =
        uncurry translate (position bullet) (rotate 90 (scale 1.5 1.5 sprite))
    | otherwise =
        uncurry translate (position bullet) (rotate 270 (scale 1.5 1.5 sprite))

buttonsToPictures :: [Button] -> [Picture]
buttonsToPictures = map buttonToPicture

buttonToPicture :: Button -> Picture
buttonToPicture button =
    pictures (color white (drawBox button) : [buttonTextToPicture button])

buttonTextToPicture :: Button -> Picture
buttonTextToPicture button = translate (x - 75) y pic
    where (x, y) = position button
          pic = scale 0.2 0.2 (text (buttonText button))

buttonText :: Button -> String
buttonText (Button _ _ Start) = "Start Game"
buttonText (Button _ _ Level1) = "Level 1"
buttonText (Button _ _ Level2) = "Level 2"
buttonText (Button _ _ Level3) = "Level 3"
buttonText (Button _ _ LevelCustom) = "Custom Level"
buttonText (Button _ _ Quit) = "Quit Game"
buttonText (Button _ _ ToHighScore) = "High Scores"
buttonText (Button _ _ Retry) = "Retry"
buttonText (Button _ _ Resume) = "Resume"
buttonText (Button _ _ ToMainMenu) = "Main Menu"
-- buttonText _ = "Button"

highScoresToPicture :: GameState -> String -> Picture
highScoresToPicture gs scores
    | menu gs == HighScores = highScoresToPicture' scores
    | otherwise = Blank

highScoresToPicture' :: String -> Picture
highScoresToPicture' scores = highScoresToPicture'' (lines scores) 0

highScoresToPicture'' :: [String] -> Int ->  Picture
highScoresToPicture'' [] _ = Blank
highScoresToPicture'' (score:scores) n =
    pictures (highScoreToPicture score n :
              [highScoresToPicture'' scores (n + 1)])

highScoreToPicture :: String -> Int -> Picture
highScoreToPicture score n = translate (-400) (300 - (fromIntegral n * 50)) (scale 0.2 0.2 pic)
    where pic = pictures (highScoreBox : [text score])

highScoreBox :: Picture
highScoreBox = Color white (polygon [(-50, -50), (-50, 220), (800, 220), (800, -50)])

-- handy for hitboxes and buttons
drawBox :: GameObject a => a -> Picture
drawBox obj = polygon [nW, nE, sE, sW]
    where (nW, nE, sE, sW) = objectCorners obj

timerToPicture :: GameState -> Picture
timerToPicture gs
    | menu gs == GameOverMenu = translate (-50) 300 (scale 0.2 0.2 gameOver)
    | menu gs == PauseMenu = translate (-50) 300 (scale 0.2 0.2 pause)
    | menu gs == VictoryMenu = translate (-50) 300 (scale 0.2 0.2 victory)
    | time gs == bossTime = translate (-50) 300 (scale 0.2 0.2 boss)
    | time gs == bossWaitTime = translate (-50) 300 (scale 0.2 0.2 bossComing)
    | otherwise = translate (-100) 300 (scale 0.2 0.2 pic)
    where pic = text ("Time until boss: " ++ show (floorFloat (time gs)))
          boss = text "Boss Battle!"
          bossComing = text "Boss battle imminent!"
          pause = text "Paused"
          gameOver = text "Game Over"
          victory = text "Victory!"

scoreToPicture :: GameState -> Picture
scoreToPicture gs = translate (-700) 300 (scale 0.2 0.2 pic)
    where pic = text ("Score: " ++ show (score gs))

livesToPicture :: GameState -> Picture
livesToPicture gs = translate 500 300 (scale 0.2 0.2 pic)
    where pic = text ("Lives left: " ++ show (playerHealth (player gs)))


-- # Helper Functions # --

-- This helper function rounds down a Float to the nearest whole number.
floorFloat :: Float -> Int
floorFloat flt = fromIntegral (floor flt)
