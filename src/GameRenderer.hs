module GameRenderer
  ( render
  ) where

import Geometry
import Rendering

import qualified Apple
import qualified GameWorld
import qualified Snake

render :: Dimensions -> GameWorld.T -> Actions
render dimensions gameWorld =
  let apple = GameWorld.apple gameWorld
      snake = GameWorld.snake gameWorld
      state = _renderState dimensions $ Snake.state snake
   in concat [_renderApple $ apple, _renderSnake $ snake, state]

_locationSize :: Int
_locationSize = 15

_renderSnake :: Snake.T -> Actions
_renderSnake snake = _renderLocations $ Snake.locations snake

_renderApple :: Apple.T -> Actions
_renderApple apple = [SetColor Red, _renderLocation $ Apple.location apple]

_renderLocations :: [Point] -> Actions
_renderLocations locations = (SetColor Green) : map _renderLocation locations

_renderLocation :: Point -> Action
_renderLocation location =
  FillRect (_toPixel location) (Dimensions _locationSize _locationSize)

_renderState :: Dimensions -> Snake.State -> Actions
_renderState _ Snake.Alive = []
_renderState (Dimensions width height) Snake.Dead =
  let x = (width `div` 2) - 110
      y = height `div` 2
   in _renderMessage (Point x y) "You have died."

_renderMessage :: Point -> String -> Actions
_renderMessage point message = [SetColor Black, FillText message point]

_toPixel :: Point -> Point
_toPixel (Point x y) = Point (x * _locationSize) (y * _locationSize)
