{-# LANGUAGE OverloadedStrings #-}

import Terminal.Game
import Logic

gameLogic :: GEnv -> World -> Event -> Either () World
gameLogic _ s (KeyPress 'z') = Right $ turnleft s
gameLogic _ s (KeyPress 'x') = Right $ turnright s
gameLogic _ s (KeyPress 'r') = Right $ restart s
gameLogic _ s (KeyPress 'a') = Right $ goeast s -- left arrow
gameLogic _ s (KeyPress 'd') = Right $ gowest s -- right arrow
gameLogic _ s (KeyPress 'w') = Right $ gonorth s -- up arrow
gameLogic _ s (KeyPress 's') = Right $ gosouth s -- down arrow
gameLogic _ _ (KeyPress 'q') = Left ()
gameLogic _ s Tick = Right $ logic s
gameLogic _ s _ = Right s

gameDraw :: GEnv -> World -> Plane
gameDraw _ = draw

-- Define the game
myGame :: StdGen -> Game World ()
myGame rng = Game
    { gTPS = 5                 -- Game ticks per second
    , gInitState = makeWorld rng -- Initial game state
    , gLogicFunction = gameLogic -- Logic function for game
    , gDrawFunction = gameDraw   -- Drawing function for game
    }

main :: IO ()
main = do
    rng <- getStdGen
    playGame (myGame rng)