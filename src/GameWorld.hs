module GameWorld
  ( T
  , make
  , dimensions
  , rectangle
  , snake
  , apple
  , tick
  , turn
  ) where

import qualified Apple
import qualified Data.List
import Geometry
import qualified Snake

data T =
  T
    { dimensions :: Dimensions
    , snake :: Snake.T
    , apples :: [Apple.T]
    }
  deriving (Show, Eq)

make :: Dimensions -> Snake.T -> [Apple.T] -> T
make dimensions snake apples = T {dimensions, snake, apples}

apple :: GameWorld.T -> Apple.T
apple gameWorld = head $ apples gameWorld

rectangle :: GameWorld.T -> Rectangle
rectangle gameWorld = Rectangle (Point 0 0) (dimensions gameWorld)

tick :: T -> T
tick gameWorld =
  let nextSnake = (_nextSnake gameWorld)
      nextApples = (_nextApples gameWorld)
   in gameWorld {snake = nextSnake, apples = nextApples}

turn :: T -> Direction -> T
turn gameWorld direction =
  if oppositeDirection direction /= (Snake.heading $ snake gameWorld)
    then gameWorld {snake = Snake.turn (snake gameWorld) direction}
    else gameWorld

_nextSnake :: GameWorld.T -> Snake.T
_nextSnake gameWorld =
  if (Snake.state $ GameWorld.snake gameWorld) == Snake.Alive
    then let moved = (Snake.move (snake gameWorld))
             maybeGrown = (_maybeGrow moved (apple gameWorld))
             maybeKilled = (_maybeKill maybeGrown (rectangle gameWorld))
          in maybeKilled
    else GameWorld.snake gameWorld

_nextApples :: GameWorld.T -> [Apple.T]
_nextApples gameWorld =
  let moved = (Snake.move (snake gameWorld))
   in if (_eatingApple moved (apple gameWorld))
        then tail $ apples gameWorld
        else apples gameWorld

_maybeGrow :: Snake.T -> Apple.T -> Snake.T
_maybeGrow snake curApple =
  if (_eatingApple snake curApple)
    then Snake.grow snake
    else snake

_maybeKill :: Snake.T -> Rectangle -> Snake.T
_maybeKill snake gameRectangle =
  if (not $ withinRectangle (Snake.location snake) gameRectangle) ||
     (_eatingSelf snake)
    then Snake.kill snake
    else snake

_eatingApple :: Snake.T -> Apple.T -> Bool
_eatingApple snake curApple =
  (Snake.location snake) == (Apple.location curApple)

_eatingSelf :: Snake.T -> Bool
_eatingSelf snake = not $ _allUnique $ Snake.locations snake

_allUnique :: Eq a => [a] -> Bool
_allUnique as = length as == (length $ Data.List.nub as)
