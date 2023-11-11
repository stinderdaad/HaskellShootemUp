{-# LANGUAGE InstanceSigs #-}

module Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import System.Random
import Data.List


-- # Classes # --

class GameObject a where
    position :: a -> Position
    size :: a -> (Int, Int)

class GameObject a => CanShoot a where
    shoot :: a -> (a, [Bullet])

class GameObject a => CanCollide a where
    hit :: a -> a
    isDead :: a -> Bool


-- # Data structures # --
type Position = Point

type Direction = Vector

--type Size = (Int, Int)

data GameState = GameState {
    menu :: Menu,
    buttons :: [Button],
    player :: Player,
    enemies :: [Enemy],
    bullets :: [Bullet],
    items :: [Item],
    walls :: [Wall],
    score :: Int,
    time :: Float,
    settings :: Settings
} deriving (Show)


data Menu = MainMenu | Playing | PauseMenu | GameOverMenu |
            VictoryMenu | HighScores | Quitting | HighScoreUpdater
    deriving (Show, Eq)

data Function = Start | ToHighScore | Quit | Retry | ToMainMenu | Resume
    deriving (Show, Eq)

data Attack = BasicAttack | ItemAttack Item
    deriving (Show, Eq)

data EnemyType = BasicEnemy | ToughEnemy | BossEnemy
    deriving (Show, Eq)

data Player = Player {
    playerPosition :: Position,
    playerDirection :: Direction,
    playerSpeed :: Float,
    playerHealth :: Int,
    playerSize :: (Int, Int),
    playerAttack :: Attack,
    playerReloadTime :: Float,
    playerTimeToNextReload :: Float
} deriving (Show)

data Enemy = Enemy {
    enemyType :: EnemyType,
    enemyPosition :: Position,
    enemyDirection :: Direction,
    enemySpeed :: Float,
    enemyHealth :: Int,
    enemySize :: (Int, Int),
    enemyAttack :: Attack,
    pointsWorth :: Int,
    enemyReloadTime :: Float,
    enemyTimeToNextReload :: Float
} deriving (Show, Eq)

data Bullet = Bullet {
    bulletPosition :: Position,
    bulletDirection :: Direction,
    bulletSpeed :: Float,
    bulletSize :: (Int, Int),
    pierce :: Int
} deriving (Show, Eq)

data Item = Item {
    itemPosition :: Position,
    itemSize :: (Int, Int),
    bulletQuantity :: Int,
    reloadTimeMultiplier :: Float,
    timer :: Float
} deriving (Show, Eq)

data Wall = Wall {
    wallPosition :: Position,
    wallSize :: (Int, Int)
} deriving (Show)

data Button = Button {
    buttonPosition :: Position,
    buttonSize :: (Int, Int),
    buttonFunction :: Function
} deriving (Show, Eq)

data Settings = Settings {
    enemySpawnRate :: Float,
    itemSpawnRate :: Float,
    enemiesInLevel :: [Enemy],
    bossInLevel :: Enemy
} deriving (Show)


-- # Instances # --

instance GameObject Player where
    position :: Player -> Position
    position = playerPosition
    size :: Player -> (Int, Int)
    size = playerSize

instance GameObject Enemy where
    position :: Enemy -> Position
    position = enemyPosition
    size :: Enemy -> (Int, Int)
    size = enemySize

instance GameObject Bullet where
    position :: Bullet -> Position
    position = bulletPosition
    size :: Bullet -> (Int, Int)
    size = bulletSize

instance GameObject Item where
    position :: Item -> Position
    position = itemPosition
    size :: Item -> (Int, Int)
    size = itemSize

instance GameObject Wall where
    position :: Wall -> Position
    position = wallPosition
    size :: Wall -> (Int, Int)
    size = wallSize

instance GameObject Button where
    position :: Button -> Position
    position = buttonPosition
    size :: Button -> (Int, Int)
    size = buttonSize

instance CanShoot Player where
    shoot :: Player -> (Player, [Bullet])
    shoot player
        | playerTimeToNextReload player > 0 = (player, [])
        | playerAttack player == BasicAttack =
            (player { playerTimeToNextReload = playerReloadTime player},
            [Bullet (playerBulletSpawn player) (1, 0) 15 (25, 15) 1])
        | otherwise = (player, [])
        where
            playerBulletSpawn player' = (x + (fromIntegral w / 2.0) + 25, y)
                where
                    (x, y) = position player'
                    (w, _) = size player'

instance CanShoot Enemy where
    shoot :: Enemy -> (Enemy, [Bullet])
    shoot enemy
        | enemyTimeToNextReload enemy > 0 = (enemy, [])
        | enemyAttack enemy == BasicAttack =
            (enemy { enemyTimeToNextReload = enemyReloadTime enemy } ,
            [Bullet (enemyBulletSpawn enemy) (-1, 0) 7.5 (25, 15) 1])
        | otherwise = (enemy, [])
        where
            enemyBulletSpawn enemy' = (x - (fromIntegral w / 2.0) - 25, y)
                where
                    (x, y) = position enemy'
                    (w, _) = size enemy'

instance CanCollide Player where
    hit :: Player -> Player
    hit player = player { playerHealth = playerHealth player - 1 }
    isDead :: Player -> Bool
    isDead player = playerHealth player <= 0

instance CanCollide Enemy where
    hit :: Enemy -> Enemy
    hit enemy = enemy { enemyHealth = enemyHealth enemy - 1 }
    isDead :: Enemy -> Bool
    isDead enemy = enemyHealth enemy <= 0

instance CanCollide Bullet where
    hit :: Bullet -> Bullet
    hit bullet = bullet { pierce = pierce bullet - 1 }
    isDead :: Bullet -> Bool
    isDead bullet = pierce bullet <= 0

instance CanCollide Wall where
    hit :: Wall -> Wall
    hit wall = wall
    isDead :: Wall -> Bool
    isDead wall = False


-- # Initialisations # --

initState :: GameState
initState = GameState {
    menu = MainMenu,
    buttons = mainMenuButtons,
    player = initPlayer,
    enemies = [],
    bullets = [],
    items = [],
    walls = defaultWalls,
    score = 0,
    time = 0,
    settings = level1
}

initLevel :: GameState
initLevel = GameState {
    menu = Playing,
    buttons = noButtons,
    player = initPlayer,
    enemies = [],
    bullets = [],
    items = [],
    walls = defaultWalls,
    score = 0,
    time = 10,
    settings = level2
}

level1 :: Settings
level1 = Settings 0.5 0.5 [basicEnemy] basicBoss

level2 :: Settings
level2 = Settings 0.75 0.75 [basicEnemy, toughEnemy] basicBoss

-- pos dir speed health size attack reloadTime timeToNextReload
initPlayer :: Player
initPlayer = Player (-600 ,0) (0, 0) 5 3 (25, 50) BasicAttack 0.2 0

basicEnemy :: Enemy
basicEnemy = Enemy BasicEnemy (800, 0) (-1, 0) 2.5 3 (25, 60) BasicAttack 50 3 0

toughEnemy :: Enemy
toughEnemy = Enemy ToughEnemy (800, 0) (-1, 0) 1 10 (50, 120) BasicAttack 200 1.5 0

basicBoss :: Enemy
basicBoss = Enemy BossEnemy (800, 0) (-1, 0) 0.2 50 (90, 180) BasicAttack 1000 1.5 0

defaultWalls :: [Wall]
defaultWalls = [Wall (0, 550) (2000, 300), -- up
                Wall (0, -550) (2000, 300), -- down
                Wall (-950, 0) (300, 800), -- left
                Wall (1100, 0) (300, 800)] -- right

startButton :: Button
startButton = Button (0, 75) (200, 50) Start

quitButton :: Button
quitButton = Button (0, -75) (200, 50) Quit

highScoreButton :: Button
highScoreButton = Button (0, 0) (200, 50) ToHighScore

retryButton :: Button
retryButton = Button (0, 75) (200, 50) Retry

mainMenuButton :: Button
mainMenuButton = Button (0, -75) (200, 50) ToMainMenu

resumeButton :: Button
resumeButton = Button (0, 75) (200, 50) Resume

noButtons :: [Button]
noButtons = []

mainMenuButtons :: [Button]
mainMenuButtons = [startButton, highScoreButton, quitButton]

gameOverButtons :: [Button]
gameOverButtons = [retryButton, mainMenuButton]

pauseButtons :: [Button]
pauseButtons = [resumeButton, mainMenuButton]

victoryButtons :: [Button]
victoryButtons = [retryButton, highScoreButton, mainMenuButton]

highScoresButtons :: [Button]
highScoresButtons = [mainMenuButton]


-- # Functions # --

getBoss :: [Enemy] -> Enemy
getBoss [] = error "No boss in list"
getBoss (enemy:enemies)
    | enemyType enemy == BossEnemy = enemy
    | otherwise = getBoss enemies

filterPlayerBullets :: [Bullet] -> [Bullet]
filterPlayerBullets bullets =
    [bullet | bullet <- bullets, bulletDirection bullet == (1, 0)]

filterEnemyBullets :: [Bullet] -> [Bullet]
filterEnemyBullets bullets = bullets \\ filterPlayerBullets bullets


objectCorners :: GameObject a => a -> (Position, Position, Position, Position)
objectCorners obj = ((x - (fromIntegral w/2.0), y + (fromIntegral h/2.0)), -- NW
                     (x + (fromIntegral w/2.0), y + (fromIntegral h/2.0)), -- NE
                     (x + (fromIntegral w/2.0), y - (fromIntegral h/2.0)), -- SE
                     (x - (fromIntegral w/2.0), y - (fromIntegral h/2.0))) -- SW
                        where (x, y) = position obj
                              (w, h) = size obj

collision :: (CanCollide a, CanCollide b) => a -> b -> a
collision obj1 obj2
    | collision' obj1 obj2 = hit obj1
    | otherwise = obj1

collision' :: (CanCollide a, CanCollide b) => a -> b -> Bool
collision' obj1 obj2 = collision'' obj1 obj2 || collision'' obj2 obj1

collision'' :: (CanCollide a, CanCollide b)  => a -> b -> Bool
collision'' obj1 obj2 =
    positionInObject cornerNW obj2 ||
    positionInObject cornerNE obj2 ||
    positionInObject cornerSE obj2 ||
    positionInObject cornerSW obj2
        where (cornerNW, cornerNE, cornerSE, cornerSW) = objectCorners obj1

positionInObject :: GameObject a => Position -> a -> Bool
positionInObject (xPos, yPos) obj =
    xPos >= xObj - (fromIntegral objWidth / 2.0)
    && xPos <= xObj + (fromIntegral objWidth / 2.0)
    && yPos >= yObj - (fromIntegral objHeight / 2.0)
    && yPos <= yObj + (fromIntegral objHeight / 2.0)
    where (xObj, yObj) = position obj
          (objWidth, objHeight) = size obj

countPoints :: [Enemy] -> Int
countPoints [] = 0
countPoints (enemy:enemies)
    | isDead enemy = pointsWorth enemy + countPoints enemies
    | otherwise = countPoints enemies
