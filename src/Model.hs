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

data Object = PlayerObject Player | EnemyObject Enemy | BulletObject Bullet | ItemObject Item -- BossObject Enemy


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

-- Show (for Testing)

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

initState :: GameState
initState = GameState {
    player = initPlayer,
    objects = [EnemyObject basicEnemy],
    score = 0,
    time = 100,
    settings = level1
}

level1 :: Settings
level1 = Settings 1 1 [basicEnemy, toughEnemy] basicBoss

allObjects :: GameState -> [Object]
allObjects gs = PlayerObject(player gs) : objects gs

objectPosition :: Object -> Position
objectPosition (PlayerObject player) = playerPosition player
objectPosition (EnemyObject enemy) = enemyPosition enemy
objectPosition (BulletObject bullet) = bulletPosition bullet
objectPosition (ItemObject item) = itemPosition item

initPlayer :: Player
initPlayer = Player (Point (-600) 0) (Vector 0 0) 3 Basic 5

basicEnemy :: Enemy
basicEnemy = Enemy (Point 400 200) (Vector 0 0) 1 (10, 10) Basic 5 50

toughEnemy :: Enemy
toughEnemy = Enemy (Point 400 200) (Vector 0 0) 3 (10, 10) Basic 5 100

basicBoss :: Enemy
basicBoss = Enemy (Point 400 200) (Vector 0 0) 20 (50, 50) Basic 5 1000

basicBullet :: Object -> Bullet
basicBullet (PlayerObject player) = Bullet (playerPosition player) (Vector 1 0) 15 (2, 2)
-- Direction should be towards players position, add later
basicBullet (EnemyObject enemy) = Bullet (enemyPosition enemy) (Vector (-1) 0) 10 (2, 2)
basicBullet _ = error "Cannot create bullet from bullet or item"
