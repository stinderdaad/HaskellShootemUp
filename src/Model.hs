module Model where

data GameState = GameState {
    player :: Player,
    objects :: [Object],
    score :: Int,
    time :: Float,
    settings :: Settings
}

data Object = PlayerObject Player | EnemyObject Enemy | BulletObject Bullet

data Attack = Basic | ItemAttack Item

data Position = Position {
    xPos :: Float,
    yPos :: Float
}

-- Should return a normalised direction vector
data Direction = Direction {
    xDir :: Float,
    yDir :: Float
}

data Player = Player {
    playerPosition :: Position,
    playerHealth :: Int,
    playerAttack :: Attack
}

data Enemy = Enemy {
    enemyPosition :: Position,
    enemyHealth :: Int,
    enemySize :: (Int, Int),
    enemyAttack :: Attack,
    pointsWorth :: Int
}

data Bullet = Bullet {
    bulletPosition :: Position,
    bulletDirection :: Direction
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

-- If i ever want to incorporate speed i can do that here
newPosition :: Position -> Direction -> Position
newPosition (Position x y) (Direction x' y') = Position (x + x') (y + y')
