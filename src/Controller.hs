module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gs
        | menuState == Playing && currentTime == (-10) = return (checkCollision gs {
                           player = updatePlayer (player gs),
                           objects = updateObjects (objects gs),
                           time = -10
                        })
        | menuState == Playing && currentTime <= 0 = return (spawnBoss (checkCollision gs{
                           player = updatePlayer (player gs),
                           objects = updateObjects (objects gs),
                           time = -10
                        }))
        | menuState == Playing =  return (checkCollision gs {
                           player = updatePlayer (player gs),
                           objects = updateObjects (objects gs),
                           time = currentTime - secs
                        })
        | otherwise = return gs
    where menuState = menu gs
          currentTime = time gs


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
        | c == 'p' && menuState == Playing = pauseGame gs
        | c == 'p' && menuState == PauseMenu = resumeGame gs
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
handleSpecialKeyDown KeySpace gs = shoot gs -- maybe auto shoot? no reason not to spam
handleSpecialKeyDown _ gs = gs

handleSpecialKeyUp :: SpecialKey -> GameState -> GameState
handleSpecialKeyUp KeyLeft gs = movePlayerRight gs
handleSpecialKeyUp KeyRight gs = movePlayerLeft gs
handleSpecialKeyUp KeyUp gs = movePlayerDown gs
handleSpecialKeyUp KeyDown gs = movePlayerUp gs
handleSpecialKeyUp _ gs = gs

-- bulletHitObject :: GameState -> Bullet -> Object -> GameState
-- bulletHitObject gs bullet (PlayerObject player) = gs { player = player { playerHealth = playerHealth player - 1 } }
-- bulletHitObject _ _ _ = undefined

-- playerDead :: GameState -> Bool
-- playerDead gs = playerHealth (player gs) <= 0

newPosition :: Position -> Direction -> Float -> Position
newPosition (Point x y) dir speed = Point (x + (dirX * speed)) (y + (dirY * speed))
    where (Vector dirX dirY) = normalizeDirection dir

normalizeDirection :: Direction -> Direction
normalizeDirection (Vector 0 0) = Vector 0 0
normalizeDirection (Vector 0 y) = Vector 0 (y / abs y)
normalizeDirection (Vector x 0) = Vector (x / abs x) 0
normalizeDirection (Vector x y) = Vector (x / magnitude) (y / magnitude)
    where magnitude = sqrt (x^2 + y^2)

addDirections :: Direction -> Direction -> Direction
addDirections (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

movePlayerLeft :: GameState -> GameState
movePlayerLeft gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (Vector (-1) 0) } }

movePlayerRight :: GameState -> GameState
movePlayerRight gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (Vector 1 0) } }

movePlayerUp :: GameState -> GameState
movePlayerUp gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (Vector 0 1) } }

movePlayerDown :: GameState -> GameState
movePlayerDown gs = gs { player = (player gs) {
    playerDirection = addDirections (playerDirection (player gs))  (Vector 0 (-1)) } }

shoot :: GameState -> GameState
shoot gs = gs { objects = objects gs ++ [BulletObject (basicBullet (PlayerObject (player gs)))] }

updatePlayer :: Player -> Player
updatePlayer player = player {
    playerPosition = newPosition (playerPosition player) (playerDirection player) (playerSpeed player) }

updateObject :: Object -> Object
updateObject (BulletObject bullet) = BulletObject bullet {
    bulletPosition = newPosition (bulletPosition bullet) (bulletDirection bullet) (bulletSpeed bullet) }
updateObject (PlayerObject player) = PlayerObject player {
    playerPosition = newPosition (playerPosition player) (playerDirection player) (playerSpeed player) }
updateObject (EnemyObject enemy) = EnemyObject enemy {
    enemyPosition = newPosition (enemyPosition enemy) (enemyDirection enemy) (enemySpeed enemy) }
updateObject (BossObject boss) = BossObject boss {
    enemyPosition = newPosition (enemyPosition boss) (enemyDirection boss) (enemySpeed boss) }
updateObject (ItemObject item) = ItemObject item

updateObjects :: [Object] -> [Object]
updateObjects = map updateObject

-- placeholder, need to figure out a nice system for collision
checkCollision :: GameState -> GameState
checkCollision gs = gs { objects = filter (not . objectHitObject (PlayerObject (player gs))) (objects gs) }

removeDeadObjects :: GameState -> GameState
removeDeadObjects gs = gs { objects = filter (not . isDead) (objects gs) }

spawnBasic :: GameState -> GameState
spawnBasic gs = gs { objects = objects gs ++ [EnemyObject basicEnemy] }

spawnTough :: GameState -> GameState
spawnTough gs = gs { objects = objects gs ++ [EnemyObject toughEnemy] }

spawnBoss :: GameState -> GameState
spawnBoss gs = gs { objects = objects gs ++ [BossObject basicBoss] }

pauseGame :: GameState -> GameState
pauseGame gs = gs { menu = PauseMenu }

resumeGame :: GameState -> GameState
resumeGame gs = gs { menu = Playing }

gameOver :: GameState -> GameState
gameOver gs = gs { menu = GameOverMenu }
