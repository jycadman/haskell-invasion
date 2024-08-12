module AI where

import Data.Fixed (mod')

import Settings

-- | Position of a game asset given as an (x,y)-coordinate pair of Floats.
type Pos = (Float, Float)

-- | Status of the game (paused or playing).
data GameMode = Paused | Playing
  deriving Eq

-- | Overall data type to manage game assets and behavior.
data GameState = GameState
  { gameMode :: GameMode   -- ^ Current game mode (paused or playing).
  , playing :: Bool        -- ^ Play button pressed flag.
  , gameStats :: GameStats -- ^ Game statistics.
  , ship :: Ship           -- ^ The ship.
  , bullets :: [Bullet]    -- ^ Bullets in the current game state.
  , aliens :: [Alien]      -- ^ Aliens in the current game state.
  , gameOver :: Bool       -- ^ Game over flag.
  }

-- | Track statistics for Alien Invasion.
data GameStats = GameStats
  { shipsLeft :: Int       -- ^ Ships left in the game.
  , level :: Int           -- ^ Current level in the game.
  , score :: Int           -- ^ Current score in the game.
  }

-- | A data type to manage the ship.
data Ship = Ship
  { shipPos :: Pos         -- ^ (x,y) position of the ship.
  , shipSpeed :: Float     -- ^ Speed of the ship.
  , movingLeft :: Bool     -- ^ Left movement flag of the ship.
  , movingRight :: Bool    -- ^ Right movement flag of the ship.
  }

-- | A data type to manage bullets fired from the ship.
data Bullet = Bullet
  { bulletPos :: Pos       -- ^ (x,y) position of a bullet.
  , bulletSpeed :: Float   -- ^ Speed of a bullet.
  }

-- | A data type to represent a single alien in the fleet.
data Alien = Alien
  { alienPos :: Pos        -- ^ (x,y) position of an alien.
  , alienSpeed :: Float    -- ^ Speed of an alien.
  , alienDir :: Float      -- ^ Direction of an alien.
  }

-- | Initialize statistics.
initGameStats :: GameStats
initGameStats = GameStats shipLimit 1 0

-- | Initialize the ship and set its starting position.
initShip :: Ship
initShip = Ship (0, shipY) initShipSpeed False False

-- | Initialize the alien fleet and set its starting position.
initAliens :: [Alien]
initAliens = [ Alien ( fstAlienX + fromIntegral col * alienSpacingX
                     , fstAlienY - fromIntegral row * alienSpacingY )
                     initFleetSpeed 0
                     | row <- fleetRows, col <- fleetCols ]

-- | Initialize the game (state).
initGameState :: GameState
initGameState = GameState Paused False initGameStats initShip [] initAliens False

-- | Updates the game's state.
update :: Float -> GameState -> GameState
update seconds gameState
  | gameMode gameState == Paused         = gameState
  | shipsLeft (gameStats gameState) <= 0 = gameState { gameOver = True }
  | isShipHit gameState                  = resetLevel gameState
  | otherwise =
    let
      gameState' = updateLevel (checkBulletAlienCollisions gameState)
      ship'      = updateShip seconds gameState'
      bullets'   = updateBullets seconds gameState'
      aliens'    = updateAliens seconds gameState'
    in gameState' { ship = ship', bullets = bullets', aliens = aliens' }

-- | Create a new bullet and add it to the bullets in the game state.
fireBullets :: GameState -> GameState
fireBullets gameState
  | length (bullets gameState) < bulletsAllowed =
    let
      (shipX, _)     = shipPos (ship gameState)
      newBulletPos   = (shipX, shipY)
      newBulletSpeed = getBulletSpeed (level (gameStats gameState))
      newBullet      = Bullet newBulletPos newBulletSpeed
    in gameState { bullets = newBullet : bullets gameState }
  | otherwise = gameState

-- | Check if a bullet has hit alien.
isBulletAlienCollision :: Bullet -> Alien -> Bool
isBulletAlienCollision bullet alien =
  let
    (bulletX, bulletY) = bulletPos bullet
    (alienX, alienY)   = alienPos alien
  in abs (bulletX - alienX) < alienWidth && abs (bulletY - alienY) < alienHeight

-- | Respond to bullet-alien collisions. Remove any bullets and aliens
--   that have collided.
checkBulletAlienCollisions :: GameState -> GameState
checkBulletAlienCollisions gameState =
  let
    currAliens       = aliens gameState
    currBullets      = bullets gameState
    survivingAliens  = filter (\a -> not $ any (`isBulletAlienCollision` a) currBullets) currAliens
    survivingBullets = filter (\b -> not $ any (isBulletAlienCollision b) currAliens) currBullets
    aliensHit        = length currAliens - length survivingAliens
    currScore        = score (gameStats gameState)
    newPoints        = aliensHit * getAlienScore (level (gameStats gameState))
    updatedGameStats = (gameStats gameState) { score = currScore + newPoints }
  in
    gameState { gameStats = updatedGameStats
              , bullets = survivingBullets
              , aliens = survivingAliens
              }

-- | Update the ship's position based on movement flags.
updateShip :: Float -> GameState -> Ship
updateShip seconds gameState =
  let
    updatedShip = ship gameState
    (shipX, _)  = shipPos updatedShip
    moveAmount  = getShipSpeed (level (gameStats gameState)) * seconds
    shipPos'
      | movingLeft updatedShip &&
        shipX >= windowLeft + shipWidth / 2 = (shipX - moveAmount, shipY)
      | movingRight updatedShip &&
        shipX <= windowRight - shipWidth / 2 = (shipX + moveAmount, shipY)
      | otherwise = (shipX, shipY)
  in updatedShip { shipPos = shipPos' }

-- | Move the bullet up the screen.
updateBullets :: Float -> GameState -> [Bullet]
updateBullets seconds gameState =
  let
    moveAmount     = getBulletSpeed (level (gameStats gameState)) * seconds
    updatedBullets = map (\b -> b { bulletPos = ( fst (bulletPos b)
                                                , snd (bulletPos b)
                                                + moveAmount )
                                  }) (bullets gameState)
  in filter (\b -> snd (bulletPos b) <= windowTop) updatedBullets

-- | Move the alien to the right, left, or down.
updateAliens :: Float -> GameState -> [Alien]
updateAliens seconds gameState =
  let
    updatedAliens  = map (\a -> a { alienPos = ( fst (alienPos a)
                                               + cos (alienDir a * pi / 180)
                                               * alienSpeed a
                                               * seconds
                                               , snd (alienPos a) )
                                  }) (aliens gameState)
    leftMostAlien  = minimum $ map (fst . alienPos) updatedAliens
    rightMostAlien = maximum $ map (fst . alienPos) updatedAliens
  in
    if leftMostAlien <= windowLeft || rightMostAlien >= windowRight
    then map (\a -> a { alienPos = ( fst (alienPos a)
                                   , snd (alienPos a) - shipHeight / 2 )
                                   , alienDir = (alienDir a + 180) `mod'` 360
                      }) updatedAliens
    else updatedAliens

-- | Update the game's level if the entire alien fleet is shot down.
updateLevel :: GameState -> GameState
updateLevel gameState
  | null (aliens gameState) =
    let
      currStats     = gameStats gameState
      newLevel      = level currStats + 1
      updatedStats  = currStats { level = newLevel }
      newAliens     = resetAliens newLevel
    in gameState { gameStats = updatedStats, bullets = [], aliens = newAliens }
  | otherwise = gameState

-- | Check if any aliens have reached the bottom of the screen,
--   and look for alien-ship collisions.
isShipHit :: GameState -> Bool
isShipHit gameState =
  let
    bottomMostAlien = minimum $ map (snd . alienPos) (aliens gameState)
    (shipX, _) = shipPos (ship gameState)
  in
    bottomMostAlien <= windowBottom + alienHeight / 2 ||
    any (\a -> abs (fst (alienPos a) - shipX) < alienWidth &&
               abs (snd (alienPos a) - shipY) < alienHeight) (aliens gameState)

-- | Resets the ship's position at the given current level.
resetShip :: Int -> Ship
resetShip currLevel = Ship (0, shipY) (getShipSpeed currLevel) False False

-- | Resets the alien fleet's position at the given current level.
resetAliens :: Int -> [Alien]
resetAliens currLevel = [ Alien ( fstAlienX + fromIntegral col * alienSpacingX
                                , fstAlienY - fromIntegral row * alienSpacingY )
                                (getFleetSpeed currLevel) 0
                                | row <- fleetRows, col <- fleetCols ]
  
-- | Respond to the ship being hit by an alien.
resetLevel :: GameState -> GameState
resetLevel gameState =
  let
    shipsLeft' = shipsLeft (gameStats gameState) - 1
    gameStats' = (gameStats gameState) { shipsLeft = shipsLeft' }
    ship'      = resetShip (level (gameStats gameState))
    aliens'    = resetAliens (level (gameStats gameState))
  in
    gameState { gameMode = Playing
              , playing = True
              , gameStats = gameStats'
              , ship = ship'
              , bullets = []
              , aliens = aliens'
              , gameOver = False
              } 
