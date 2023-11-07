{-# LANGUAGE InstanceSigs #-}

module Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import System.Random


-- # Classes # --

class GameObject a where
    position :: a -> Position
    size :: a -> (Int, Int)
    isDead :: a -> Bool

class GameObject a => CanShoot a where
    shoot :: a -> [Bullet]

class GameObject a => CanBeHit a where
    hit :: a -> a


-- # Data structures # --
type Position = Point

type Direction = Vector

data GameState = GameState {
    menu :: Menu,
    buttons :: [Button],
    player :: Player,
    enemies :: [Enemy],
    bullets :: [Bullet],
    items :: [Item],
    score :: Int,
    time :: Float,
    settings :: Settings
}


data Menu = MainMenu | Playing | PauseMenu | GameOverMenu |
            VictoryMenu | HighScores | Quitting
    deriving (Show, Eq)

data Function = Start | ToHighScore | Quit | Retry | ToMainMenu | Resume
    deriving (Show, Eq)

data Attack = Basic | ItemAttack Item
    deriving (Show, Eq)

data Player = Player {
    playerPosition :: Position,
    playerDirection :: Direction,
    playerSpeed :: Float,
    playerHealth :: Int,
    playerSize :: (Int, Int),
    playerAttack :: Attack,
    playerTimeToReload :: Float
} deriving (Show)

data Enemy = Enemy {
    enemyPosition :: Position,
    enemyDirection :: Direction,
    enemySpeed :: Float,
    enemyHealth :: Int,
    enemySize :: (Int, Int),
    enemyAttack :: Attack,
    pointsWorth :: Int,
    enemyTimeToReload :: Float
} deriving (Show)

data Bullet = Bullet {
    bulletPosition :: Position,
    bulletDirection :: Direction,
    bulletSpeed :: Float,
    bulletSize :: (Int, Int),
    reloadTime :: Float,
    pierce :: Int
} deriving (Show)

data Item = Item {
    itemPosition :: Position,
    itemSize :: (Int, Int),
    bulletQuantity :: Int,
    reloadTimeMultiplier :: Float,
    timer :: Float
} deriving (Show, Eq)

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
    isDead :: Player -> Bool
    isDead player = playerHealth player <= 0

instance GameObject Enemy where
    position :: Enemy -> Position
    position = enemyPosition
    size :: Enemy -> (Int, Int)
    size = enemySize
    isDead :: Enemy -> Bool
    isDead enemy = enemyHealth enemy <= 0

instance GameObject Bullet where
    position :: Bullet -> Position
    position = bulletPosition
    size :: Bullet -> (Int, Int)
    size = bulletSize
    isDead :: Bullet -> Bool
    isDead _ = False

instance GameObject Item where
    position :: Item -> Position
    position = itemPosition
    size :: Item -> (Int, Int)
    size = itemSize
    isDead :: Item -> Bool
    isDead _ = False

instance GameObject Button where
    position :: Button -> Position
    position = buttonPosition
    size :: Button -> (Int, Int)
    size = buttonSize
    isDead :: Button -> Bool
    isDead _ = False

instance CanShoot Player where
    shoot :: Player -> [Bullet]
    shoot player
        | playerTimeToReload player > 0 = []
        | playerAttack player == Basic = [Bullet (playerBulletSpawn player) (1, 0) 15 (40, 20) 0.2 1]
        | otherwise = []
        where
            playerBulletSpawn player' = (x + (fromIntegral w / 2.0) + 25, y)
                where
                    (x, y) = position player'
                    (w, _) = size player'

instance CanShoot Enemy where
    shoot :: Enemy -> [Bullet]
    shoot enemy
        | enemyTimeToReload enemy > 0 = []
        | enemyAttack enemy == Basic = [Bullet (enemyBulletSpawn enemy) (-1, 0) 10 (2, 2) 0.5 1]
        | otherwise = []
        where
            enemyBulletSpawn enemy' = (x - (fromIntegral w / 2.0) - 25, y)
                where
                    (x, y) = position enemy'
                    (w, _) = size enemy'

instance CanBeHit Player where
    hit :: Player -> Player
    hit player = player { playerHealth = playerHealth player - 1 }

instance CanBeHit Enemy where
    hit :: Enemy -> Enemy
    hit enemy = enemy { enemyHealth = enemyHealth enemy - 1 }

instance CanBeHit Bullet where
    hit :: Bullet -> Bullet
    hit bullet = bullet { pierce = pierce bullet - 1 }


-- instance Show GameState where
--     show gs = "GameState { time: " ++ show (time gs)
--               -- ++ ", player: " ++ show (player gs)
--               ++ ", objects: " ++ show (objects gs)
--               -- ++ ", score: " ++ show (score gs)
--               -- ++ ", settings: " ++ show (settings gs)
--               ++ " }"

-- instance Show Player where
--     show player = "Player { position: " ++ show (playerPosition player)
--                   ++ ", direction: " ++ show (playerDirection player)
--                   ++ ", health: " ++ show (playerHealth player)
--                   ++ ", attack: " ++ show (playerAttack player)
--                   ++ ", speed: " ++ show (playerSpeed player)
--                   ++ " }"

-- instance Show Object where
--     show (PlayerObject player) = show player
--     show (EnemyObject enemy) = show enemy
--     show (BossObject boss) = show boss
--     show (BulletObject bullet) = show bullet
--     show (ItemObject item) = show item

-- instance Show Enemy where
--     show enemy = "Enemy { position: " ++ show (enemyPosition enemy)
--                  ++ ", health: " ++ show (enemyHealth enemy)
--                  ++ ", size: " ++ show (enemySize enemy)
--                  ++ ", attack: " ++ show (enemyAttack enemy)
--                  ++ ", speed: " ++ show (enemySpeed enemy)
--                  ++ ", pointsWorth: " ++ show (pointsWorth enemy)
--                  ++ " }"

-- instance Eq Enemy where
--     (==) enemy1 enemy2 =
--         enemyPosition enemy1 == enemyPosition enemy2 &&
--         enemySpeed enemy1 == enemySpeed enemy2 &&
--         enemyHealth enemy1 == enemyHealth enemy2 &&
--         enemySize enemy1 == enemySize enemy2 &&
--         enemyAttack enemy1 == enemyAttack enemy2 &&
--         pointsWorth enemy1 == pointsWorth enemy2

-- instance Eq Attack where
--     (==) Basic Basic = True
--     (==) (ItemAttack item1) (ItemAttack item2) = item1 == item2
--     (==) _ _ = False

-- instance Show Bullet where
--     show bullet = "Bullet { position: " ++ show (bulletPosition bullet)
--                   ++ ", direction: " ++ show (bulletDirection bullet)
--                   ++ ", speed: " ++ show (bulletSpeed bullet)
--                   ++ ", size: " ++ show (bulletSize bullet)
--                   ++ " }"

-- instance Show Item where
--     show item = "Item { position: " ++ show (itemPosition item)
--                 ++ ", size: " ++ show (itemSize item)
--                 ++ ", bulletQuantity: " ++ show (bulletQuantity item)
--                 ++ ", reloadTimeMultiplier: " ++ show (reloadTimeMultiplier item)
--                 ++ " }"

-- instance Show Settings where
--     show settings = "Settings { enemySpawnRate: " ++ show (enemySpawnRate settings)
--                     ++ ", itemSpawnRate: " ++ show (itemSpawnRate settings)
--                     ++ ", enemies: " ++ show (enemies settings)
--                     ++ ", boss: " ++ show (boss settings)
--                     ++ " }"


-- # Initialisations # --

initState :: GameState
initState = GameState {
    menu = MainMenu,
    buttons = mainMenuButtons,
    player = initPlayer,
    enemies = [],
    bullets = [],
    items = [],
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
    score = 0,
    time = 3,
    settings = level2
}

level1 :: Settings
level1 = Settings 0.5 0.5 [basicEnemy] basicBoss

level2 :: Settings
level2 = Settings 1.0 1.0 [basicEnemy, toughEnemy] basicBoss

initPlayer :: Player
initPlayer = Player (-600 ,0) (0, 0) 5 3 (25, 50) Basic 0

basicEnemy :: Enemy
basicEnemy = Enemy (800, 0) (-1, 0) 5 3 (25, 60) Basic 50 0

toughEnemy :: Enemy
toughEnemy = Enemy (800, 0) (-1, 0) 2.5 10 (50, 120) Basic 200 0

basicBoss :: Enemy
basicBoss = Enemy (800, 0) (-1, 0) 0.5 50 (90, 180) Basic 1000 0

-- basicBullet :: Object -> Bullet
-- basicBullet (PlayerObject player) = Bullet playerBulletSpawn (Vector 1 0) 15 (40, 20) 0.2
--     where playerBulletSpawn = Point (x + (fromIntegral w/2.0) + 25) y
--           (Point x y) = playerPosition player
--           (w, _) = playerSize player
-- basicBullet (EnemyObject enemy) = Bullet enemyBulletSpawn (Vector (-1) 0) 10 (2, 2) 0.5
--     where enemyBulletSpawn = Point (x - (fromIntegral w/2.0) - 25) y
--           (Point x y) = enemyPosition enemy
--           (w, _) = enemySize enemy
-- basicBullet (BossObject boss) = Bullet bossBulletSpawn (Vector (-1) 0) 10 (2, 2) 0.1
--     where bossBulletSpawn = Point (x - (fromIntegral w/2.0) - 25) y
--           (Point x y) = enemyPosition boss
--           (w, _) = enemySize boss
-- basicBullet _ = error "Cannot create bullet from bullet or item"

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

objectCorners :: GameObject a => a -> (Position, Position, Position, Position)
objectCorners obj = ((x - (fromIntegral w/2.0), y + (fromIntegral h/2.0)), -- NW
                     (x + (fromIntegral w/2.0), y + (fromIntegral h/2.0)), -- NE
                     (x + (fromIntegral w/2.0), y - (fromIntegral h/2.0)), -- SE
                     (x - (fromIntegral w/2.0), y - (fromIntegral h/2.0))) -- SW
                        where (x, y) = position obj
                              (w, h) = size obj

-- shoot :: Object -> [Object]
-- shoot obj@(PlayerObject player)
--     | playerTimeToReload player > 0 = []
--     | playerAttack player == Basic = [BulletObject (basicBullet obj)]
--     | otherwise = []
-- shoot obj@(EnemyObject enemy)
--     | enemyTimeToReload enemy > 0 = [obj]
--     | enemyAttack enemy == Basic = EnemyObject enemy{ enemyTimeToReload = reloadTime (basicBullet obj) } :
--                                    [BulletObject (basicBullet obj)]
--     | otherwise = [obj]
-- shoot obj@(BossObject boss)
--     | enemyTimeToReload boss > 0 = [obj]
--     | enemyAttack boss == Basic = BossObject boss{ enemyTimeToReload = reloadTime (basicBullet obj) } :
--                                   [BulletObject (basicBullet obj)]
--     | otherwise = [obj]
-- shoot obj = [obj]

-- playerShoot :: GameState -> GameState
-- playerShoot gs
--         | playerTimeToReload player' > 0 = gs
--         | playerAttack player' == Basic = gs {
--             objects = objects ++ shoot (PlayerObject player'),
--         }
--     where player' = player gs

-- not including buttons
-- allObjects :: GameState -> [Object]
-- allObjects gs = PlayerObject(player gs) : objects gs

-- objectPosition :: Object -> Position
-- objectPosition (PlayerObject player) = playerPosition player
-- objectPosition (EnemyObject enemy) = enemyPosition enemy
-- objectPosition (BossObject boss) = enemyPosition boss
-- objectPosition (BulletObject bullet) = bulletPosition bullet
-- objectPosition (ItemObject item) = itemPosition item
-- objectPosition (ButtonObject button) = buttonPosition button
-- objectPosition _ = Point 0 0

-- objectSize :: Object -> (Int, Int)
-- objectSize (PlayerObject player) = playerSize player
-- objectSize (EnemyObject enemy) = enemySize enemy
-- objectSize (BossObject boss) = enemySize boss
-- objectSize (BulletObject bullet) = bulletSize bullet
-- objectSize (ItemObject item) = itemSize item
-- objectSize (ButtonObject button) = buttonSize button
-- objectSize _ = (0, 0)

-- isPlayer :: Object -> Bool
-- isPlayer (PlayerObject _) = True
-- isPlayer _ = False

-- objectCornerNW :: Object -> Position
-- objectCornerNW obj = Point (x - (fromIntegral w/2.0)) (y + (fromIntegral h/2.0))
--                     where (Point x y) = objectPosition obj
--                           (w, h) = objectSize obj

-- objectCornerNE :: Object -> Position
-- objectCornerNE obj = Point (x + (fromIntegral w/2.0)) (y + (fromIntegral h/2.0))
--                     where (Point x y) = objectPosition obj
--                           (w, h) = objectSize obj

-- objectCornerSE :: Object -> Position
-- objectCornerSE obj = Point (x + (fromIntegral w/2.0)) (y - (fromIntegral h/2.0))
--                     where (Point x y) = objectPosition obj
--                           (w, h) = objectSize obj

-- objectCornerSW :: Object -> Position
-- objectCornerSW obj = Point (x - (fromIntegral w/2.0)) (y - (fromIntegral h/2.0))
--                     where (Point x y) = objectPosition obj
--                           (w, h) = objectSize obj

-- objectHitObject :: Object -> Object -> Bool
-- objectHitObject obj1 obj2 = objectHitObject' obj1 obj2 ||
--                             objectHitObject' obj2 obj1

-- objectHitObject' :: Object -> Object -> Bool
-- objectHitObject' obj1 obj2 =
--     positionInObject (objectCornerNW obj1) obj2 ||
--     positionInObject (objectCornerNE obj1) obj2 ||
--     positionInObject (objectCornerSE obj1) obj2 ||
--     positionInObject (objectCornerSW obj1) obj2

-- positionInObject :: Position -> Object -> Bool
-- positionInObject pos obj = positionInObject' pos (objectPosition obj) (objectSize obj)

-- positionInObject' :: Position -> Position -> (Int, Int) -> Bool
-- positionInObject' (Point xPos yPos) (Point xObj yObj) (objWidth, objHeight) =
--     xPos >= xObj - (fromIntegral objWidth / 2.0)
--     && xPos <= xObj + (fromIntegral objWidth / 2.0)
--     && yPos >= yObj - (fromIntegral objHeight / 2.0)
--     && yPos <= yObj + (fromIntegral objHeight / 2.0)

-- playerHit :: Player -> Player
-- playerHit player = player { playerHealth = playerHealth player - 1 }

-- enemyHit :: Enemy -> Enemy
-- enemyHit enemy = enemy { enemyHealth = enemyHealth enemy - 1 }

-- isDead :: Object -> Bool
-- isDead (PlayerObject player) = playerHealth player <= 0
-- isDead (EnemyObject enemy) = enemyHealth enemy <= 0
-- isDead (BossObject boss) = enemyHealth boss <= 0
-- isDead (BulletObject bullet) = False
-- isDead (ItemObject item) = False
-- isDead DeadObject = True

-- countPoints :: [Object] -> Int
-- countPoints [] = 0
-- countPoints (EnemyObject enemy:xs)
--         | isDead (EnemyObject enemy) = pointsWorth enemy + countPoints xs
--         | otherwise = countPoints xs
-- countPoints (BossObject boss:xs)
--         | isDead (BossObject boss) = pointsWorth boss + countPoints xs
--         | otherwise = countPoints xs
-- countPoints (_:xs) = countPoints xs
