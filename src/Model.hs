{-# LANGUAGE InstanceSigs #-}

module Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import System.Random
import Data.List


-- # Constants # --

bossTime :: Float
bossTime = -10

bossWaitTime :: Float
bossWaitTime = -1


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
    animations :: [AnimationState],
    walls :: [Wall],
    score :: Int,
    time :: Float,
    settings :: Settings
} deriving (Show)


data Menu = MainMenu | LevelSelect | LoadCustomLevel | Playing | PauseMenu |
            GameOverMenu | VictoryMenu | HighScores | Quitting | HighScoreUpdater
    deriving (Show, Eq)

data Function = Start | Level1 | Level2 | Level3 | LevelCustom | ToHighScore | Quit |
                Retry | ToMainMenu | Resume
    deriving (Show, Eq)

data Attack = BasicAttack | TargetedAttack Player | ItemAttack Item | DualAttack Attack Attack
    deriving (Show, Eq)

data EnemyType = BasicEnemy | ToughEnemy | SmartEnemy | BossEnemy
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
} deriving (Show, Eq)

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

data AnimationState = AnimationState {
    amountOfSprites :: Int,
    animationPosition :: Position,
    animationCurrentSprite :: Int,
    framesPerSprite :: Int,
    framesTillNextSprite :: Int,
    animationOver :: Bool
} deriving (Show)

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
        | otherwise =
            (enemy { enemyTimeToNextReload = enemyReloadTime enemy } ,
            enemyAttackToBullets enemy (enemyAttack enemy))

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
    animations = [],
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
    animations = [],
    walls = defaultWalls,
    score = 0,
    time = 100,
    settings = level2
}

level1 :: Settings
level1 = Settings 0.5 0.5 [basicEnemy] basicBoss

level2 :: Settings
level2 = Settings 0.5 0.5 [basicEnemy, toughEnemy] dualBoss

level3 :: Settings
level3 = Settings 0.5 0.5 [basicEnemy, toughEnemy, smartEnemy] basicBossFastAttack

-- pos dir speed health size attack reloadTime timeToNextReload
initPlayer :: Player
initPlayer = Player (-600 ,0) (0, 0) 5 3 (25, 50) BasicAttack 0.2 0

basicEnemy :: Enemy
basicEnemy = Enemy BasicEnemy (800, 0) (-1, 0) 2.5 1 (25, 60) BasicAttack 50 3 0

toughEnemy :: Enemy
toughEnemy = Enemy ToughEnemy (800, 0) (-1, 0) 1 5 (50, 120) (DualAttack BasicAttack BasicAttack) 200 1.5 0

smartEnemy :: Enemy
smartEnemy = Enemy SmartEnemy (800, 0) (-1, 0) 1 1 (25, 60) (TargetedAttack initPlayer) 200 1.5 0

basicBoss :: Enemy
basicBoss = Enemy BossEnemy (850, 0) (-1, 0) 0.2 30 (90, 180) (TargetedAttack initPlayer) 1000 0.5 0

dualBoss :: Enemy
dualBoss = Enemy BossEnemy (850, 0) (-1, 0) 0.2 30 (90, 180) (DualAttack (TargetedAttack initPlayer) (TargetedAttack initPlayer)) 1000 0.5 0

basicBossFastAttack :: Enemy
basicBossFastAttack = Enemy BossEnemy (850, 0) (-1, 0) 0.2 30 (90, 180) (TargetedAttack initPlayer) 1000 0.2 0

wallBoss :: Enemy
wallBoss = Enemy BossEnemy (950, 0) (-1, 0) 2 75 (90, 799) BasicAttack 1000 999 999

defaultWalls :: [Wall]
defaultWalls = [Wall (0, 550) (2000, 300), -- up
                Wall (0, -550) (2000, 300), -- down
                Wall (-950, 0) (300, 800), -- left
                Wall (1100, 0) (300, 800)] -- right

newAnimationState :: Int -> Position -> AnimationState
newAnimationState amountOfSprites position =
    AnimationState amountOfSprites position 1 5 5 False

startButton :: Button
startButton = Button (0, 75) (200, 50) Start

retryButton :: Button
retryButton = Button (0, 75) (200, 50) Retry

resumeButton :: Button
resumeButton = Button (0, 75) (200, 50) Resume

level1Button :: Button
level1Button = Button (0, 150) (200, 50) Level1

level2Button :: Button
level2Button = Button (0, 75) (200, 50) Level2

level3Button :: Button
level3Button = Button (0, 0) (200, 50) Level3

highScoreButton :: Button
highScoreButton = Button (0, 0) (200, 50) ToHighScore

levelCustomButton :: Button
levelCustomButton = Button (0, -75) (200, 50) LevelCustom

mainMenuButton :: Button
mainMenuButton = Button (0, -150) (200, 50) ToMainMenu

quitButton :: Button
quitButton = Button (0, -75) (200, 50) Quit

noButtons :: [Button]
noButtons = []

mainMenuButtons :: [Button]
mainMenuButtons = [startButton, highScoreButton, quitButton]

levelSelectButtons :: [Button]
levelSelectButtons = [level1Button, level2Button, level3Button, levelCustomButton, mainMenuButton]

gameOverButtons :: [Button]
gameOverButtons = [retryButton, mainMenuButton]

pauseButtons :: [Button]
pauseButtons = [resumeButton, mainMenuButton]

victoryButtons :: [Button]
victoryButtons = [retryButton, highScoreButton, mainMenuButton]

highScoresButtons :: [Button]
highScoresButtons = [mainMenuButton]


-- # Functions # --

direction :: Position -> Position -> Direction
direction pos1 pos2 = normalizeDirection (x' - x, y' - y)
    where
        (x, y) = pos1
        (x', y') = pos2

normalizeDirection :: Vector -> Vector
normalizeDirection (0, 0) = (0, 0)
normalizeDirection (0, y) = (0, y / abs y)
normalizeDirection (x, 0) = (x / abs x, 0)
normalizeDirection (x, y) =  (x / magnitude, y / magnitude)
    where magnitude = sqrt (x^2 + y^2)

getBoss :: [Enemy] -> Enemy
getBoss [] = error "No boss in list"
getBoss (enemy:enemies)
    | enemyType enemy == BossEnemy = enemy
    | otherwise = getBoss enemies

updateTargetedAttacks :: GameState -> GameState
updateTargetedAttacks gs = gs { enemies = newEnemies }
    where
        newEnemies = map (updateTargetedAttack gs) (enemies gs)

updateTargetedAttack :: GameState -> Enemy -> Enemy
updateTargetedAttack gs enemy = enemy { enemyAttack = updateTargetedAttack' gs (enemyAttack enemy) }

updateTargetedAttack' :: GameState -> Attack -> Attack
updateTargetedAttack' gs (TargetedAttack target) = TargetedAttack (player gs)
updateTargetedAttack' gs (DualAttack attack1 attack2) = DualAttack (updateTargetedAttack' gs attack1) (updateTargetedAttack' gs attack2)
updateTargetedAttack' gs attack = attack

enemyAttackToBullets :: Enemy -> Attack -> [Bullet]
enemyAttackToBullets enemy BasicAttack =
    [Bullet (enemyBulletSpawn enemy) (-1, 0) 7.5 (25, 15) 1]
enemyAttackToBullets enemy (TargetedAttack target) =
    [Bullet (enemyBulletSpawn enemy) (direction (enemyBulletSpawn enemy) (position target)) 7.5 (25, 15) 1]
enemyAttackToBullets enemy (DualAttack (TargetedAttack target) (TargetedAttack target')) =
    Bullet (x, y + 50) (direction (x, y + 100) (position target)) 7.5 (25, 15) 1 :
    [Bullet (x, y - 50) (direction (x, y - 100) (position target')) 7.5 (25, 15) 1]
        where (x, y) = enemyBulletSpawn enemy
enemyAttackToBullets enemy (DualAttack attack1 attack2) =
    map (moveBulletBy (0, 30)) (enemyAttackToBullets enemy attack1) ++
    map (moveBulletBy (0, -30)) (enemyAttackToBullets enemy attack2)

moveBulletBy :: Position -> Bullet -> Bullet
moveBulletBy (x, y) bullet = bullet { bulletPosition = (x + x', y + y') }
    where (x', y') = bulletPosition bullet

enemyBulletSpawn :: Enemy -> Position
enemyBulletSpawn enemy = (x - (fromIntegral w / 2.0) - 25, y)
    where
        (x, y) = position enemy
        (w, _) = size enemy

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
