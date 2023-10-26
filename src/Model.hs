module Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import System.Random

-- Data structures

data GameState = GameState {
    menu :: Menu,
    player :: Player,
    objects :: [Object],
    score :: Int,
    time :: Float,
    settings :: Settings
}

data Menu = MainMenu | Playing | PauseMenu |
            GameOverMenu | VictoryMenu | HighScores
    deriving (Show, Eq)

data Object = PlayerObject Player |
              EnemyObject Enemy |
              BossObject Enemy |
              BulletObject Bullet |
              ItemObject Item |
              ButtonObject Button |
              DeadObject

data Button = Button {
    buttonPosition :: Position,
    buttonSize :: (Int, Int),
    buttonFunction :: Function
}

data Function = Start | HighScore | Quit | Retry | ToMainMenu | Resume
    deriving (Show, Eq)

data Attack = Basic | ItemAttack Item
    deriving (Show)

data Position = Point Float Float
    deriving (Show)

data Direction = Vector Float Float
    deriving (Show)

data Player = Player {
    playerPosition :: Position,
    playerDirection :: Direction,
    playerSpeed :: Float,
    playerHealth :: Int,
    playerSize :: (Int, Int),
    playerAttack :: Attack
}

data Enemy = Enemy {
    enemyPosition :: Position,
    enemyDirection :: Direction,
    enemySpeed :: Float,
    enemyHealth :: Int,
    enemySize :: (Int, Int),
    enemyAttack :: Attack,
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
    reloadTimeMultiplier :: Float,
    timer :: Float
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
    show (BossObject boss) = show boss
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

-- # Initialisations # --

initState :: GameState
initState = GameState {
    menu = Playing, -- should be main menu later
    player = initPlayer,
    objects = [EnemyObject basicEnemy], -- should be empty when spawning works
    score = 0,
    time = 3,
    settings = level1
}

level1 :: Settings
level1 = Settings 1 1 [basicEnemy, toughEnemy] basicBoss

allObjects :: GameState -> [Object]
allObjects gs = PlayerObject(player gs) : objects gs

objectPosition :: Object -> Position
objectPosition (PlayerObject player) = playerPosition player
objectPosition (EnemyObject enemy) = enemyPosition enemy
objectPosition (BossObject boss) = enemyPosition boss
objectPosition (BulletObject bullet) = bulletPosition bullet
objectPosition (ItemObject item) = itemPosition item
objectPosition _ = Point 0 0

objectSize :: Object -> (Int, Int)
objectSize (PlayerObject player) = playerSize player
objectSize (EnemyObject enemy) = enemySize enemy
objectSize (BossObject boss) = enemySize boss
objectSize (BulletObject bullet) = bulletSize bullet
objectSize (ItemObject item) = itemSize item
objectSize _ = (0, 0)

initPlayer :: Player
initPlayer = Player (Point (-600) 0) (Vector 0 0) 5 3 (25, 50) Basic

-- enemies y should be random
basicEnemy :: Enemy
basicEnemy = Enemy (Point 800 0) (Vector (-1) 0) 5 1 (25, 60) Basic 50

toughEnemy :: Enemy
toughEnemy = Enemy (Point 800 0) (Vector (-1) 0) 3 3 (25, 50) Basic 100

basicBoss :: Enemy
basicBoss = Enemy (Point 800 0) (Vector (-1) 0) 0.5 20 (90, 180) Basic 1000

basicBullet :: Object -> Bullet
basicBullet (PlayerObject player) = Bullet playerBulletSpawn (Vector 1 0) 15 (40, 20)
    where playerBulletSpawn = Point (x + (fromIntegral w/2.0) + 25) y
          (Point x y) = playerPosition player
          (w, _) = playerSize player
-- Direction should be towards players position, add later
-- basicBullet (EnemyObject enemy) = Bullet (enemyPosition enemy) (Vector (-1) 0) 10 (2, 2)
basicBullet _ = error "Cannot create bullet from bullet or item"

startButton :: Button
startButton = Button (Point 0 0) (200, 100) Start

quitButton :: Button
quitButton = Button (Point 0 (-150)) (200, 100) Quit

highScoreButton :: Button
highScoreButton = Button (Point 0 (-300)) (200, 100) HighScore

retryButton :: Button
retryButton = Button (Point 0 0) (200, 100) Retry

mainMenuButton :: Button
mainMenuButton = Button (Point 0 (-300)) (200, 100) ToMainMenu

resumeButton :: Button
resumeButton = Button (Point 0 0) (200, 100) Resume

-- # Functions # --

isPlayer :: Object -> Bool
isPlayer (PlayerObject _) = True
isPlayer _ = False

objectCornerNW :: Object -> Position
objectCornerNW obj = Point (x - (fromIntegral w/2.0)) (y + (fromIntegral h/2.0))
                    where (Point x y) = objectPosition obj
                          (w, h) = objectSize obj

objectCornerNE :: Object -> Position
objectCornerNE obj = Point (x + (fromIntegral w/2.0)) (y + (fromIntegral h/2.0))
                    where (Point x y) = objectPosition obj
                          (w, h) = objectSize obj

objectCornerSE :: Object -> Position
objectCornerSE obj = Point (x + (fromIntegral w/2.0)) (y - (fromIntegral h/2.0))
                    where (Point x y) = objectPosition obj
                          (w, h) = objectSize obj

objectCornerSW :: Object -> Position
objectCornerSW obj = Point (x - (fromIntegral w/2.0)) (y - (fromIntegral h/2.0))
                    where (Point x y) = objectPosition obj
                          (w, h) = objectSize obj

objectHitObject :: Object -> Object -> Bool
objectHitObject obj1 obj2 = objectHitObject' obj1 obj2 ||
                            objectHitObject' obj2 obj1

objectHitObject' :: Object -> Object -> Bool
objectHitObject' obj1 obj2 =
    positionInObject (objectCornerNW obj1) obj2 ||
    positionInObject (objectCornerNE obj1) obj2 ||
    positionInObject (objectCornerSE obj1) obj2 ||
    positionInObject (objectCornerSW obj1) obj2

positionInObject :: Position -> Object -> Bool
positionInObject pos obj = positionInObject' pos (objectPosition obj) (objectSize obj)

positionInObject' :: Position -> Position -> (Int, Int) -> Bool
positionInObject' (Point xPos yPos) (Point xObj yObj) (objWidth, objHeight) =
    xPos >= xObj - (fromIntegral objWidth / 2.0)
    && xPos <= xObj + (fromIntegral objWidth / 2.0)
    && yPos >= yObj - (fromIntegral objHeight / 2.0)
    && yPos <= yObj + (fromIntegral objHeight / 2.0)

playerHit :: Player -> Player
playerHit player = player { playerHealth = playerHealth player - 1 }

enemyHit :: Enemy -> Enemy
enemyHit enemy = enemy { enemyHealth = enemyHealth enemy - 1 }

isDead :: Object -> Bool
isDead (PlayerObject player) = playerHealth player <= 0
isDead (EnemyObject enemy) = enemyHealth enemy <= 0
isDead (BossObject boss) = enemyHealth boss <= 0
isDead (BulletObject bullet) = False
isDead (ItemObject item) = False
isDead DeadObject = True
