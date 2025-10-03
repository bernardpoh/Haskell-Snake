{-# LANGUAGE OverloadedStrings #-}

import Terminal.Game
import Logic

gameLogic :: GEnv -> World -> Event -> Either () World
gameLogic _ s (KeyPress 'a') = Right $ goleft s
gameLogic _ s (KeyPress 'd') = Right $ goright s
gameLogic _ s (KeyPress 'r') = Right $ restart s
gameLogic _ s (KeyPress 'q') = Left ()
gameLogic _ s Tick = Right $ logic s
gameLogic _ s _ = Right $ s

gameDraw :: GEnv -> World -> Plane
gameDraw _ s = draw s

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