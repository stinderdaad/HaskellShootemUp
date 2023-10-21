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

data Object = PlayerObject Player | EnemyObject Enemy | BulletObject Bullet

data Attack = Basic | ItemAttack Item
    deriving (Show)

data Position = Point Float Float
    deriving (Show)

data Direction = Vector Float Float
    deriving (Show)

data Player = Player {
    playerPosition :: Position,
    playerHealth :: Int,
    playerAttack :: Attack,
    playerSpeed :: Float
}

data Enemy = Enemy {
    enemyPosition :: Position,
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
              ++ ", player: " ++ show (player gs)
              ++ ", objects: " ++ show (objects gs)
              ++ ", score: " ++ show (score gs)
              ++ ", settings: " ++ show (settings gs)
              ++ " }"

instance Show Player where
    show player = "Player { position: " ++ show (playerPosition player)
                  ++ ", health: " ++ show (playerHealth player)
                  ++ ", attack: " ++ show (playerAttack player)
                  ++ ", speed: " ++ show (playerSpeed player)
                  ++ " }"

instance Show Object where
    show (PlayerObject player) = show player
    show (EnemyObject enemy) = show enemy
    show (BulletObject bullet) = show bullet

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
initPlayer = Player (Point (-600) 0) 3 Basic 10

basicEnemy :: Enemy
basicEnemy = Enemy (Point 400 200) 1 (10, 10) Basic 10 50

toughEnemy :: Enemy
toughEnemy = Enemy (Point 400 200) 3 (10, 10) Basic 10 100

basicBoss :: Enemy
basicBoss = Enemy (Point 400 200) 20 (50, 50) Basic 10 1000

level1 :: Settings
level1 = Settings 1 1 [basicEnemy, toughEnemy] basicBoss

initState :: GameState
initState = GameState {
    player = initPlayer,
    objects = [PlayerObject initPlayer],
    score = 0,
    time = 100,
    settings = level1
}

basicBullet :: Object -> Bullet
basicBullet (PlayerObject player) = Bullet (playerPosition player) (Vector 1 0) 5 (2, 2)
-- Direction should be towards players position, add later
basicBullet (EnemyObject enemy) = Bullet (enemyPosition enemy) (Vector (-1) 0) 5 (2, 2)
basicBullet _ = error "Cannot create bullet from bullet"

bulletHitObject :: GameState -> Bullet -> Object -> GameState
bulletHitObject gs bullet (PlayerObject player) = gs { player = player { playerHealth = playerHealth player - 1 } }

playerDead :: GameState -> Bool
playerDead gs = playerHealth (player gs) <= 0

-- Direction should be normalized vector
newPosition :: Position -> Direction -> Float -> Position
newPosition (Point x y) (Vector vx vy) speed = Point (x + (vx * speed)) (y + (vy * speed))

movePlayerLeft :: GameState -> GameState
movePlayerLeft gs = gs { player = (player gs) { playerPosition = newPosition (playerPosition (player gs)) (Vector (-1) 0) (playerSpeed (player gs))} }

movePlayerRight :: GameState -> GameState
movePlayerRight gs = gs { player = (player gs) { playerPosition = newPosition (playerPosition (player gs)) (Vector 1 0) (playerSpeed (player gs))} }

movePlayerUp :: GameState -> GameState
movePlayerUp gs = gs { player = (player gs) { playerPosition = newPosition (playerPosition (player gs)) (Vector 0 1) (playerSpeed (player gs))} }

movePlayerDown :: GameState -> GameState
movePlayerDown gs = gs { player = (player gs) { playerPosition = newPosition (playerPosition (player gs)) (Vector 0 (-1)) (playerSpeed (player gs))} }

shoot :: GameState -> GameState
shoot gs = gs { objects = objects gs ++ [BulletObject (basicBullet (PlayerObject (player gs)))] }

updateObject :: Object -> Object
updateObject (BulletObject bullet) = BulletObject bullet { bulletPosition = newPosition (bulletPosition bullet) (bulletDirection bullet) (bulletSpeed bullet) }
updateObject obj = obj

updateObjects :: [Object] -> [Object]
updateObjects = map updateObject
