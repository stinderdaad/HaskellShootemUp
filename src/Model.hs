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

data Object = PlayerObject Player | EnemyObject Enemy | BulletObject Bullet

data Attack = Basic | ItemAttack Item

data Position = Point Float Float

data Direction = Vector Float Float

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

-- Functions

initPlayer :: Player
initPlayer = Player (Point 200 200) 3 Basic 2

basicEnemy :: Enemy
basicEnemy = Enemy (Point 400 200) 1 (10, 10) Basic 2 50

toughEnemy :: Enemy
toughEnemy = Enemy (Point 400 200) 3 (10, 10) Basic 2 100

basicBoss :: Enemy
basicBoss = Enemy (Point 400 200) 20 (50, 50) Basic 2 1000

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
