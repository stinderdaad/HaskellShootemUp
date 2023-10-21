module Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import System.Random

-- Data structures

data GameState = GameState {
    player :: Player,
    objects :: [Object],
    score :: Int,
    time :: Float,
    settings :: Settings
}

data Menu = MainMenu | PauseMenu | GameOverMenu | HighScores
    deriving (Show)

data Object = PlayerObject Player | EnemyObject Enemy | BulletObject Bullet | ItemObject Item


data Attack = Basic | ItemAttack Item
    deriving (Show)

data Position = Point Float Float
    deriving (Show)

data Direction = Vector Float Float
    deriving (Show)

data Player = Player {
    playerPosition :: Position,
    playerDirection :: Direction,
    playerHealth :: Int,
    playerAttack :: Attack,
    playerSpeed :: Float
}

data Enemy = Enemy {
    enemyPosition :: Position,
    enemyDirection :: Direction,
    enemyHealth :: Int,
    enemySize :: (Int, Int),
    enemyAttack :: Attack,
    enemySpeed :: Float,
    pointsWorth :: Int
}

data Bullet = Bullet {
    bulletPosition :: Position,
    bulletDirection :: Direction,
    bulletSpeed :: Float,
    bulletSize :: (Int, Int)
}

data Item = Item {
    itemPosition :: Position,
    itemSize :: (Int, Int),
    bulletQuantity :: Int,
    reloadTimeMultiplier :: Float
}

data Settings = Settings {
    enemySpawnRate :: Float,
    itemSpawnRate :: Float,
    enemies :: [Enemy],
    boss :: Enemy
}

-- Show

instance Show GameState where
    show gs = "GameState { time: " ++ show (time gs)
              -- ++ ", player: " ++ show (player gs)
              ++ ", objects: " ++ show (objects gs)
              -- ++ ", score: " ++ show (score gs)
              -- ++ ", settings: " ++ show (settings gs)
              ++ " }"

instance Show Player where
    show player = "Player { position: " ++ show (playerPosition player)
                  ++ ", direction: " ++ show (playerDirection player)
                  ++ ", health: " ++ show (playerHealth player)
                  ++ ", attack: " ++ show (playerAttack player)
                  ++ ", speed: " ++ show (playerSpeed player)
                  ++ " }"

instance Show Object where
    show (PlayerObject player) = show player
    show (EnemyObject enemy) = show enemy
    show (BulletObject bullet) = show bullet
    show (ItemObject item) = show item

instance Show Enemy where
    show enemy = "Enemy { position: " ++ show (enemyPosition enemy)
                 ++ ", health: " ++ show (enemyHealth enemy)
                 ++ ", size: " ++ show (enemySize enemy)
                 ++ ", attack: " ++ show (enemyAttack enemy)
                 ++ ", speed: " ++ show (enemySpeed enemy)
                 ++ ", pointsWorth: " ++ show (pointsWorth enemy)
                 ++ " }"

instance Show Bullet where
    show bullet = "Bullet { position: " ++ show (bulletPosition bullet)
                  ++ ", direction: " ++ show (bulletDirection bullet)
                  ++ ", speed: " ++ show (bulletSpeed bullet)
                  ++ ", size: " ++ show (bulletSize bullet)
                  ++ " }"

instance Show Item where
    show item = "Item { position: " ++ show (itemPosition item)
                ++ ", size: " ++ show (itemSize item)
                ++ ", bulletQuantity: " ++ show (bulletQuantity item)
                ++ ", reloadTimeMultiplier: " ++ show (reloadTimeMultiplier item)
                ++ " }"

instance Show Settings where
    show settings = "Settings { enemySpawnRate: " ++ show (enemySpawnRate settings)
                    ++ ", itemSpawnRate: " ++ show (itemSpawnRate settings)
                    ++ ", enemies: " ++ show (enemies settings)
                    ++ ", boss: " ++ show (boss settings)
                    ++ " }"

-- Functions

initPlayer :: Player
initPlayer = Player (Point (-600) 0) (Vector 0 0) 3 Basic 5

basicEnemy :: Enemy
basicEnemy = Enemy (Point 400 200) (Vector 0 0) 1 (10, 10) Basic 5 50

toughEnemy :: Enemy
toughEnemy = Enemy (Point 400 200) (Vector 0 0) 3 (10, 10) Basic 5 100

basicBoss :: Enemy
basicBoss = Enemy (Point 400 200) (Vector 0 0) 20 (50, 50) Basic 5 1000

level1 :: Settings
level1 = Settings 1 1 [basicEnemy, toughEnemy] basicBoss

initState :: GameState
initState = GameState {
    player = initPlayer,
    objects = [EnemyObject basicEnemy],
    score = 0,
    time = 100,
    settings = level1
}

basicBullet :: Object -> Bullet
basicBullet (PlayerObject player) = Bullet (playerPosition player) (Vector 1 0) 15 (2, 2)
-- Direction should be towards players position, add later
basicBullet (EnemyObject enemy) = Bullet (enemyPosition enemy) (Vector (-1) 0) 10 (2, 2)
basicBullet _ = error "Cannot create bullet from bullet or item"

bulletHitObject :: GameState -> Bullet -> Object -> GameState
bulletHitObject gs bullet (PlayerObject player) = gs { player = player { playerHealth = playerHealth player - 1 } }
bulletHitObject _ _ _ = undefined

playerDead :: GameState -> Bool
playerDead gs = playerHealth (player gs) <= 0

-- Direction should be normalized vector
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

-- updatePlayerObject :: GameState -> GameState
-- updatePlayerObject gs = gs { objects = map (updatePlayerObject' (player gs)) (objects gs) }

-- updatePlayerObject' :: Player -> Object -> Object
-- updatePlayerObject'  player (PlayerObject playerObj) = PlayerObject playerObj {
--     playerPosition = playerPosition player,
--     playerDirection = playerDirection player,
--     playerHealth = playerHealth player,
--     playerAttack = playerAttack player,
--     playerSpeed = playerSpeed player
-- }
-- updatePlayerObject' _ obj = obj

updatePlayer :: Player -> Player
updatePlayer player = player { playerPosition = newPosition (playerPosition player) (playerDirection player) (playerSpeed player) }

updateObject :: Object -> Object
updateObject (BulletObject bullet) = BulletObject bullet { bulletPosition = newPosition (bulletPosition bullet) (bulletDirection bullet) (bulletSpeed bullet) }
updateObject (PlayerObject player) = PlayerObject player { playerPosition = newPosition (playerPosition player) (playerDirection player) (playerSpeed player) }
updateObject (EnemyObject enemy) = EnemyObject enemy { enemyPosition = newPosition (enemyPosition enemy) (enemyDirection enemy) (enemySpeed enemy) }
updateObject (ItemObject item) = ItemObject item

updateObjects :: [Object] -> [Object]
updateObjects = map updateObject
