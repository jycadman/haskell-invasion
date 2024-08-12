module Settings where

import Graphics.Gloss ( Color, red, makeColorI ) 

-- | Window settings.
-- Width, height, and offset of the game window.
windowWidth, windowHeight, windowOffset :: Int
windowWidth  = 1200
windowHeight = 800
windowOffset = 100

-- Background color.
bgColor :: Color
bgColor = makeColorI 230 230 230 0

-- Number of simulation steps (in FPS) to take for each second of real time.
fps :: Int
fps = 60 

-- x- and y-coordinates of left, right, bottom,
-- and top limits of the game window.
windowLeft, windowRight, windowBottom, windowTop :: Float
windowLeft   = -(fromIntegral windowWidth / 2)
windowRight  = fromIntegral windowWidth / 2
windowBottom = -(fromIntegral windowHeight / 2)
windowTop    = fromIntegral windowHeight / 2

-- | Ship settings.
-- Speed of the ship.
initShipSpeed :: Float
initShipSpeed = 400

-- Number of ships allowed in a game.
shipLimit :: Int
shipLimit = 3

-- Width and height of the ship.
shipWidth, shipHeight :: Float
shipWidth  = 60
shipHeight = 48

-- y-coordinate of the ship (is a constant).
shipY :: Float
shipY = windowBottom + shipHeight

-- | Bullet settings.
-- Speed of a bullet.
initBulletSpeed :: Float
initBulletSpeed = 400

-- The radius of a bullet.
bulletRad :: Float
bulletRad = 5

-- The color of a bullet.
bulletColor :: Color
bulletColor = red

-- Number of bullets allowed on the screen at a time.
bulletsAllowed :: Int
bulletsAllowed = 3

-- | Alien settings.
-- Width and height of an alien.
alienWidth, alienHeight :: Float
alienWidth  = 60
alienHeight = 58

-- x- and y-coordinates of the top-left initial alien in the fleet.
-- Starting point for building the alien fleet.
fstAlienX, fstAlienY :: Float
fstAlienX = windowLeft + alienWidth / 2 + alienWidth
fstAlienY = windowTop - alienHeight / 2 - alienHeight

-- Speed of the alien fleet.
initFleetSpeed :: Float
initFleetSpeed = 125

-- Number of rows and columns of the alien fleet.
fleetRows, fleetCols :: [Int]
fleetRows = [0..3]
fleetCols = [0..9]

-- Horizontal and vertical spacing between aliens in a row and column.
alienSpacingX, alienSpacingY :: Float
alienSpacingX = alienWidth * 2
alienSpacingY = alienHeight * 2

-- | Scorekeeping settings.
-- Score value of (shooting down) one alien.
alienScore :: Int
alienScore = 50

-- | Speed and score scalar values.
speedUpScale, scoreScale :: Float
speedUpScale = 1.1
scoreScale   = 1.5

-- | Dynamic game settings.
--   Increase speed settings and alien point values.
getShipSpeed :: Int -> Float
getShipSpeed level = initShipSpeed * (speedUpScale ** fromIntegral (level - 1))

getBulletSpeed :: Int -> Float
getBulletSpeed level = initBulletSpeed * (speedUpScale ** fromIntegral (level - 1))

getFleetSpeed :: Int -> Float
getFleetSpeed level = initFleetSpeed * (speedUpScale ** fromIntegral (level - 1))

getAlienScore :: Int -> Int
getAlienScore level = round $ fromIntegral alienScore * (scoreScale ** fromIntegral (level - 1))
