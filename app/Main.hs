module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import AI
import Settings

-- | Create a display window.
window :: Display
window = InWindow "Alien Invasion" (windowWidth, windowHeight) (windowOffset, windowOffset)

-- | Draws the game's current state. 
drawGame :: Picture -> Picture -> GameState -> Picture
drawGame shipImage alienImage gameState =
  let
    drawBtn        = Color green $ Translate 0 0 $ rectangleSolid 200 70 <> Scale 0.25 0.25 (Color red (Translate (-110) (-45) (Text "Play")))
    drawShip       = uncurry Translate (shipPos (ship gameState)) shipImage
    drawBullets    = map (\b -> uncurry Translate (bulletPos b) $ Color bulletColor $ Circle bulletRad) (bullets gameState)
    drawAliens     = map (\a -> uncurry Translate (alienPos a) alienImage) (aliens gameState)
    drawShipsLeft  = map (\i -> Translate (windowLeft + (shipWidth / 2) + (fromIntegral i * shipWidth)) (windowTop - shipHeight) shipImage) [0 .. shipsLeft (gameStats gameState) - 1]
    drawScore      = Color red $ Translate (windowRight - shipWidth * 3) (windowTop - 40) $ Scale 0.15 0.15 $ Text ("Score: " ++ show (score (gameStats gameState)))
    drawLevel      = Color red $ Translate (windowRight - shipWidth * 3) (windowTop - 60) $ Scale 0.15 0.15 $ Text ("Level: " ++ show (level (gameStats gameState)))
    printGameOver  = Color red $ Translate (-180) 30 $ Scale 0.5 0.5 $ Text "Game Over!"
    printHighScore = Color blue $ Translate (-180) (-30) $ Scale 0.3 0.3 $ Text ("High Score: " ++ show (score (gameStats gameState)))
    printLevel     = Color blue $ Translate (-180) (-80) $ Scale 0.3 0.3 $ Text ("Level: " ++ show (level (gameStats gameState)))
  in
    case gameMode gameState of
      Paused ->
        if not (playing gameState)
        then Pictures [drawBtn]
        else Pictures []
      Playing ->
        if gameOver gameState
        then Pictures [printGameOver, printHighScore, printLevel]
        else Pictures (drawShip : drawBullets ++ drawAliens ++ drawShipsLeft ++ [drawScore] ++ [drawLevel])

-- | Respond to keypresses and mouse events.
handleEvents :: Event -> GameState -> GameState
handleEvents (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
  | gameMode gameState == Paused && isInsideButton x y = gameState { gameMode = Playing, playing = True }
  where
    isInsideButton mx my = mx >= -100 && mx <= 100 && my >= -35 && my <= 35
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) gameState
  | gameMode gameState == Playing = fireBullets gameState
  | otherwise = gameState
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) gameState
  | gameMode gameState == Playing = gameState { ship = (ship gameState) { movingLeft = True } }
  | otherwise = gameState
handleEvents (EventKey (SpecialKey KeyLeft) Up _ _) gameState
  | gameMode gameState == Playing = gameState { ship = (ship gameState) { movingLeft = False } }
  | otherwise = gameState
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) gameState
  | gameMode gameState == Playing = gameState { ship = (ship gameState) { movingRight = True } }
  | otherwise = gameState
handleEvents (EventKey (SpecialKey KeyRight) Up _ _) gameState
  | gameMode gameState == Playing = gameState { ship = (ship gameState) { movingRight = False } }
  | otherwise = gameState
handleEvents _ gameState = gameState

main :: IO ()
main = do
  shipImage <- loadBMP "resources/ship.bmp"
  alienImage <- loadBMP "resources/alien.bmp"
  play window bgColor fps initGameState (drawGame shipImage alienImage) handleEvents update
