{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

module Logic where

import Control.Lens hiding ((#))
import Linear.V2
import Terminal.Game

class GameState a where
    initstate::a
    logic::a->a
    goleft::a->a
    goright::a->a
    restart::a->a
    restart = id
    draw::a->Plane

data World = World {
    _snake :: Snake
    , _score :: Int
    , _gameover :: Bool
    , _fruits :: [V2 Int]
    , _gen :: StdGen
    , _highscore :: Int
}

data Snake = Snake {
    _pos :: V2 Int
    , _direction :: V2 Int
    , _len :: Int
    , _maxlen :: Int
    , _body :: [V2 Int]
    , _died :: Bool
}

makeLenses ''Snake
makeLenses ''World

height::Int
height = 20
width::Int
width = 20

makeWorld :: StdGen -> World
makeWorld rng = initstate & gen .~ rng & addFruit & addFruit & addFruit

instance GameState World where
    initstate = World initstate 0 False [] (mkStdGen 0) 0
    logic = checkCollisions . (snake %~ logic)
    goleft = snake %~ goleft
    goright = snake %~ goright
    restart w = if w^.gameover then makeWorld (w^.gen) & highscore .~  w^.score else w
    draw w = vcat [scene, textbox]
        where
            scoretext = "Score: " ++ show (w^.score) ++ if w^.highscore == 0 then "" else "\nHigh score: " ++ show (w^.highscore)
            finalscoretext = if w^.score > w^.highscore then "\nNew high score!" else "Final score: " ++ show (w^.score) ++ "\nHigh score: " ++ show (w^.highscore)
            gameovertext ="Game over!\n" ++ finalscoretext ++ "\npress 'r' to restart"
            bottomtext = if w^.gameover then gameovertext else scoretext
            textbox = textBox height 4 bottomtext
            scene =
                draw (w^.snake)
                & foldr (.) id [(p^._x+1, p^._y+1) % cell '@' # color Red Vivid | p <- w^.fruits]


checkCollisions::World->World
checkCollisions w = w
    & fruits .~ []
    & foldr (.) id [if w^.snake.pos == fruit then (snake.maxlen %~ (+2)) . (score %~ (+1)) . addFruit else fruits %~ (fruit:) | fruit <- w^.fruits]
    & gameover .~ w^.snake.died

addFruit::World -> World
addFruit w = w & gen .~ newGen & fruits %~ (p:)
    where
        occupied = hitbox (w^.snake) ++ w^.fruits
        allPositions = [V2 x y | x <- [0..width-1], y <- [0..height-1]]
        freePositions = filter (`notElem` occupied) allPositions
        (p, newGen) = pickRandom freePositions (w^.gen)

class Hitbox a where
    hitbox::a->[V2 Int]

instance Hitbox Snake where
    hitbox s = s^.pos : take (s^.len) (s^.body)

moveSnake::Snake->Snake
moveSnake s = checkedSnake
    where
        newPos = s^.pos + s^.direction & _x %~ (`mod` width) & _y %~ (`mod` height)
        newBody = s^.pos : s^.body
        newLen = if s^.len < s^.maxlen then s^.len + 1 else s^.len
        updatedSnake = s & pos .~ newPos & body .~ newBody & len .~ newLen
        checkedSnake = case elem newPos $ take (s^.len) newBody of
            True -> s & died .~ True & direction .~ V2 0 0
            False -> updatedSnake

instance GameState Snake where
    initstate = Snake (V2 5 5) (V2 0 1) 0 0 [] False
    logic = moveSnake
    goleft = direction %~ perp
    goright = direction %~ negate . perp
    draw s =
        box height width '.'
        & foldr (.) id [(p1^._x+1, p1^._y+1) % cell (bodyShape (p2-p0)) | (p0, p1, p2) <- zip3 (s^.pos:s^.body) (s^.body) (s^.body&drop 1) & take (s^.len)] 
        & (s^.pos._x+1, s^.pos._y+1) % cell headShape
        where

        headShape::Char
        headShape = case s^.direction of
            V2 1 0 -> 'v'
            V2 (-1) 0 -> '^'
            V2 0 1 -> '>'
            V2 0 (-1) -> '<'
            _ -> 'o'

        bodyShape::V2 Int->Char
        bodyShape v' = case v' of
            v | parallel v (V2 1 1) -> '\\'
            v | parallel v (V2 (-1) 1) -> '/'
            v | parallel v (V2 0 1) -> '-'
            v | parallel v (V2 1 0) -> '|'
            _ -> '*'
            where
                parallel v1 v2 = crossZ v1 v2 == 0
