module Controller where

import Model
import Data.Maybe
import System.Exit
import System.Random
import System.Directory
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Text.ParserCombinators.ReadP (string)



-- # Impure # --

step :: Float -> GameState -> IO GameState
step secs gs
        | menuState == Quitting = exitSuccess
        | menuState == HighScoreUpdater = do
            fullHighScoreUpdater gs
            return (victory gs)
        | menuState == LoadCustomLevel = do
            customSettings <- loadSettings
            return (initLevel { settings = stringToSettings customSettings })
        | menuState == Playing && currentTime == bossTime =
            print gs >>
            return (updateGs gs {
                           player = updatePlayer secs (player gs),
                           enemies = updateEnemies secs (enemies gs),
                           animations = updateAnimationStates (animations gs),
                           bullets = updateBullets (bullets gs),
                           time = bossTime
                        })
        | menuState == Playing && currentTime <= 0 && null (enemies gs)=
            --print gs >>
            return ((updateGs . spawnBoss) gs{
                           player = updatePlayer secs (player gs),
                           enemies = updateEnemies secs (enemies gs),
                           animations = updateAnimationStates (animations gs),
                           bullets = updateBullets (bullets gs),
                           time = bossTime
                        })
        | menuState == Playing && currentTime <= 0 =
            --print gs >>
            return (updateGs gs {
                           player = updatePlayer secs (player gs),
                           enemies = updateEnemies secs (enemies gs),
                           animations = updateAnimationStates (animations gs),
                           bullets = updateBullets (bullets gs),
                           time = bossWaitTime
                        })
        | menuState == Playing = do
            randomFloat1 <- randomIO :: IO Float
            randomFloat2 <- randomIO :: IO Float
            --print gs
            print (show (length (bullets gs)))
            return ((updateGs . spawnEnemiesItems randomFloat1 randomFloat2) gs {
                           player = updatePlayer secs (player gs),
                           enemies = updateEnemies secs (enemies gs),
                           animations = updateAnimationStates (animations gs),
                           bullets = updateBullets (bullets gs),
                           time = currentTime - secs
                        })
        | otherwise = return gs
    where menuState = menu gs
          currentTime = time gs
          updateGs = removeDeadObjects .
                     checkBossDead .
                     awardPoints .
                     checkPlayerDead .
                     deadEnemiesToAnimations .
                     checkAllCollisions .
                     shooting .
                     updateTargetedAttacks

input :: Event -> GameState -> IO GameState
input event gs = do
    -- putStrLn $ "Received event: " ++ show event
    return $ case event of
        EventKey (Char c) Down _ _ -> handleCharKeyDown c gs
        EventKey (Char c) Up _ _ -> handleCharKeyUp c gs
        EventKey (SpecialKey key) Down _ _ -> handleSpecialKeyDown key gs
        EventKey (SpecialKey key) Up _ _ -> handleSpecialKeyUp key gs
        EventKey (MouseButton LeftButton) Down _ pos -> checkButtonPress gs pos
        _ -> gs

-- this is duplicate code, but I did not want to risk dependency issues
loadHighScores :: IO String
loadHighScores = readFile "data/HighScores.txt"

writeHighScores :: GameState -> String -> IO ()
writeHighScores gs highScores = writeFile "data/HighScoresTemp.txt" (updateHighScores gs highScores)

-- somehow it works like this but not directly because the resource is locked
updateHighScoreFile :: IO ()
updateHighScoreFile = do
    highScores <- readFile "data/HighScoresTemp.txt"
    writeFile "data/HighScores.txt" highScores
    removeFile "data/HighScoresTemp.txt"

fullHighScoreUpdater :: GameState -> IO ()
fullHighScoreUpdater gs = do
    highScores <- loadHighScores
    writeHighScores gs highScores
    updateHighScoreFile

loadSettings :: IO String
loadSettings = readFile "data/CustomSettings.txt"

stringToSettings :: String -> Settings
stringToSettings string = Settings {
    enemySpawnRate = read (head settingsList) :: Float,
    itemSpawnRate = read (settingsList !! 1) :: Float,
    enemiesInLevel = map stringToEnemy (words (settingsList !! 2)),
    bossInLevel = stringToEnemy (settingsList !! 3)
}
    where settingsList = lines string

stringToEnemy :: String -> Enemy
stringToEnemy "basicEnemy" = basicEnemy
stringToEnemy "toughEnemy" = toughEnemy
stringToEnemy "smartEnemy" = smartEnemy
stringToEnemy "basicBoss" = basicBoss
stringToEnemy "basicBossFastAttack" = basicBossFastAttack
stringToEnemy "dualBoss" = dualBoss
stringToEnemy "wallBoss" = wallBoss
stringToEnemy _ = error "Invalid enemy type"


-- # Input Functions # --

handleCharKeyDown :: Char -> GameState -> GameState
handleCharKeyDown c gs
        | c == 'a' = movePlayerLeft gs
        | c == 'd' = movePlayerRight gs
        | c == 'w' = movePlayerUp gs
        | c == 's' = movePlayerDown gs
        | c == 'p' && menuState == Playing = pauseGame gs
        | c == 'p' && menuState == PauseMenu = playGame gs
        | otherwise = gs
    where menuState = menu gs

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


-- # Pure Functions # --
-- Not sure how many of these should actually be in Model

newPosition :: Position -> Direction -> Float -> Position
newPosition (x, y) dir speed = (x + (dirX * speed), y + (dirY * speed))
    where (dirX, dirY) = normalizeDirection dir

addDirections :: Direction -> Direction -> Direction
addDirections (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

movePlayerLeft :: GameState -> GameState
movePlayerLeft gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (-1, 0) } }

movePlayerRight :: GameState -> GameState
movePlayerRight gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (1, 0) } }

movePlayerUp :: GameState -> GameState
movePlayerUp gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (0, 1) } }

movePlayerDown :: GameState -> GameState
movePlayerDown gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (0, -1) } }

shooting :: GameState -> GameState
shooting gs | menu gs == Playing = gs {
                player = fst playerShoot,
                enemies = map fst enemyShoot,
                bullets = bullets gs ++ snd playerShoot ++ concatMap snd enemyShoot
                                     }
            | otherwise = gs
    where playerShoot = shoot (player gs)
          enemyShoot = map shoot (enemies gs)

updatePlayer :: Float -> Player -> Player
updatePlayer secs player = player {
    playerPosition = newPosition (playerPosition player) (playerDirection player) (playerSpeed player),
    playerTimeToNextReload = playerTimeToNextReload player - secs
}

updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies secs = map (updateEnemy secs)

updateEnemy :: Float -> Enemy -> Enemy
updateEnemy secs enemy = enemy {
    enemyPosition = newPosition (enemyPosition enemy) (enemyDirection enemy) (enemySpeed enemy),
    enemyTimeToNextReload = enemyTimeToNextReload enemy - secs
}

updateBullets :: [Bullet] -> [Bullet]
updateBullets = map updateBullet

updateBullet :: Bullet -> Bullet
updateBullet bullet = bullet { bulletPosition = newPosition (bulletPosition bullet) (bulletDirection bullet) (bulletSpeed bullet) }

updateAnimationStates :: [AnimationState] -> [AnimationState]
updateAnimationStates = map updateAnimationState

updateAnimationState :: AnimationState -> AnimationState
updateAnimationState animation
    | animationCurrentSprite animation == amountOfSprites animation = animation { animationOver = True }
    | framesTillNextSprite animation <= 0 = animation {
        animationCurrentSprite = animationCurrentSprite animation + 1,
        framesTillNextSprite = framesPerSprite animation }
    | otherwise = animation {
        framesTillNextSprite = framesTillNextSprite animation - 1 }

spawnEnemiesItems :: Float -> Float -> GameState -> GameState
spawnEnemiesItems randomFloat1 randomFloat2 gs
        | elem smartEnemy enemies' &&
          (randomFloat1/enemySpawnRate') < 0.01
          = spawnSmart randomPos gs
        | elem toughEnemy enemies' &&
          (randomFloat1/enemySpawnRate') < 0.03
          = spawnTough randomPos gs
        | elem basicEnemy enemies' &&
          (randomFloat1/enemySpawnRate') < 0.1
          = spawnBasic randomPos gs
        | otherwise = gs
    where randomPos = randomFloat2 * 800 - 400
          itemSpawnRate' = itemSpawnRate (settings gs)
          enemySpawnRate' = enemySpawnRate (settings gs)
          enemies' = enemiesInLevel (settings gs)

-- this function should be a lot prettier, its a mess right now
checkAllCollisions :: GameState -> GameState
checkAllCollisions gs = gs {
    player = head (collideWithObjects playerAfterEnemyCollision (filterEnemyBullets bulletsInGame)),
    enemies = collideWithObjects enemiesInGame (filterPlayerBullets bulletsInGame),
    bullets = collideWithObjects (filterEnemyBullets bulletsInGame) [player gs] ++
              collideWithObjects (filterPlayerBullets bulletsInGame) enemiesInGame
}
    where playerAfterWallCollision = collideWithObjects [player gs] (walls gs)
          playerAfterEnemyCollision = collideWithObjects playerAfterWallCollision (enemies gs)
          bulletsInGame = collideWithObjects (filterEnemyBullets bulletsInGame') (filterPlayerBullets bulletsInGame') ++
                          collideWithObjects (filterPlayerBullets bulletsInGame') (filterEnemyBullets bulletsInGame')
          bulletsInGame' = collideWithObjects (bullets gs) (walls gs)
          enemiesInGame = collideWithObjects (enemies gs) (walls gs)

collideWithObjects :: (CanCollide a, CanCollide b) => [a] -> [b] -> [a]
collideWithObjects objs1 objs2 = map (\obj -> foldl collision obj objs2) objs1

removeDeadObjects :: GameState -> GameState
removeDeadObjects = removeDeadBullets . removeDeadEnemies . removeOverAnimations

removeDeadEnemies :: GameState -> GameState
removeDeadEnemies gs = gs { enemies = filter (not . isDead) (enemies gs) }

removeDeadBullets :: GameState -> GameState
removeDeadBullets gs = gs { bullets = filter (not . isDead) (bullets gs) }

removeOverAnimations :: GameState -> GameState
removeOverAnimations gs = gs { animations = filter (not . animationOver) (animations gs) }

checkPlayerDead :: GameState -> GameState
checkPlayerDead gs
        | isDead (player gs) = gameOver gs
        | otherwise = gs

checkBossDead :: GameState -> GameState
checkBossDead gs
        | time gs == -10 && isDead (getBoss (enemies gs)) = highScoreUpdater gs
        | otherwise = gs

deadEnemiesToAnimations :: GameState -> GameState
deadEnemiesToAnimations gs = gs {
    animations = map enemyToAnimation (filter isDead (enemies gs)) ++ animations gs }

enemyToAnimation :: Enemy -> AnimationState
enemyToAnimation enemy = newAnimationState 3 (enemyPosition enemy)

awardPoints :: GameState -> GameState
awardPoints gs = gs { score = score gs + countPoints (enemies gs) }

updateHighScores :: GameState -> String -> String
updateHighScores gs scores =
    unlines (take 10 (sortScores (lines scores ++ [show (score gs)])))

sortScores :: [String] -> [String]
sortScores [] = []
sortScores (x:xs) = sortScores smaller ++ [x] ++ sortScores larger
    where smaller = [a | a <- xs, (read a :: Int) >= (read x :: Int)]
          larger = [b | b <- xs, (read b :: Int) < (read x :: Int)]

spawnBasic :: Float -> GameState -> GameState
spawnBasic yPos gs = gs { enemies = enemies gs ++ [basicEnemy { enemyPosition = (800, yPos) }] }

spawnTough :: Float -> GameState -> GameState
spawnTough yPos gs = gs { enemies = enemies gs ++ [toughEnemy { enemyPosition = (800, yPos) }] }

spawnSmart :: Float -> GameState -> GameState
spawnSmart yPos gs = gs { enemies = enemies gs ++ [smartEnemy { enemyPosition = (800, yPos) }] }

spawnBoss :: GameState -> GameState
spawnBoss gs =  gs { enemies = enemies gs ++ [bossInLevel (settings gs)] }

checkButtonPress :: GameState -> Position -> GameState
checkButtonPress gs pos
        | isNothing button = gs
        | otherwise = buttonPressed gs (fromJust button)
    where button = checkButtonPress' gs pos

checkButtonPress' :: GameState -> Position -> Maybe Button
checkButtonPress' gs = checkButtonPress'' (buttons gs)

checkButtonPress'' :: [Button] -> Position -> Maybe Button
checkButtonPress'' [] _ = Nothing
checkButtonPress'' (x:xs) pos
        | positionInObject pos x = Just x
        | otherwise = checkButtonPress'' xs pos

buttonPressed :: GameState -> Button -> GameState
buttonPressed gs (Button _ _ Start) = playGame gs
buttonPressed gs (Button _ _ Level1) = initLevel { settings = level1}
buttonPressed gs (Button _ _ Level2) = initLevel { settings = level2}
buttonPressed gs (Button _ _ Level3) = initLevel { settings = level3}
buttonPressed gs (Button _ _ LevelCustom) = customLevel gs
buttonPressed gs (Button _ _ Quit) = quitGame gs
buttonPressed gs (Button _ _ ToHighScore) = goToHighScores gs
buttonPressed gs (Button _ _ Retry) = playGame gs
buttonPressed gs (Button _ _ Resume) = resumeGame gs
buttonPressed gs (Button _ _ ToMainMenu) = goToMainMenu gs
-- buttonPressed gs _ = gs


-- # Menu Functions # --

playGame :: GameState -> GameState
playGame gs = gs { menu = LevelSelect, buttons = levelSelectButtons }

customLevel :: GameState -> GameState
customLevel gs = gs { menu = LoadCustomLevel, buttons = noButtons }

pauseGame :: GameState -> GameState
pauseGame gs = gs { menu = PauseMenu, buttons = pauseButtons }

resumeGame :: GameState -> GameState
resumeGame gs = gs { menu = Playing, buttons = noButtons }

goToMainMenu :: GameState -> GameState
goToMainMenu gs = initState

goToHighScores :: GameState -> GameState
goToHighScores gs = gs { menu = HighScores, buttons = highScoresButtons }

gameOver :: GameState -> GameState
gameOver gs = gs { menu = GameOverMenu, buttons = gameOverButtons }

highScoreUpdater :: GameState -> GameState
highScoreUpdater gs = gs { menu = HighScoreUpdater, buttons = noButtons }

victory :: GameState -> GameState
victory gs = gs { menu = VictoryMenu, buttons = victoryButtons }

quitGame :: GameState -> GameState
quitGame gs = gs { menu = Quitting, buttons = noButtons }