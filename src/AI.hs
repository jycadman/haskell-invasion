module AI where

data GameState = GameState
  { ship :: Ship,
    bullets :: [Bullet]
  }

data Ship = Ship {shipPos :: (Int, Int)}

data Bullet = Bullet {pos :: (Int, Int)}
