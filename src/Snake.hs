module Snake
  ( T
  , State(..)
  , location
  , locations
  , state
  , heading
  , body
  , make
  , move
  , turn
  , grow
  , kill
  ) where

import GHC.Generics (Generic)
import Generic.Random
import Geometry
import Test.QuickCheck

data T =
  T
    { location :: Point
    , state :: State
    , heading :: Direction
    , body :: Body
    }
  deriving (Show, Eq)

data State
  = Alive
  | Dead
  deriving (Show, Eq, Generic)

type Body = [Direction]

make :: Point -> State -> Direction -> Body -> T
make location state heading body = T {location, state, heading, body}

locations :: T -> [Point]
locations snake = _locations (location snake) (body snake)

move :: T -> T
move snake = snake {location = _moveLocation snake, body = _moveBody snake}

turn :: T -> Direction -> T
turn snake direction = snake {heading = direction}

grow :: T -> T
grow snake =
  let b = body snake
   in snake {body = concat [b, [last b]]}

kill :: T -> T
kill snake = snake {state = Dead}

_locations :: Point -> Body -> [Point]
_locations origin (h:body) =
  let adjacent = (adjacentPoint (oppositeDirection h) origin)
   in origin : _locations adjacent body
_locations _ [] = []

_moveLocation :: T -> Point
_moveLocation snake = adjacentPoint (heading snake) (location snake)

_moveBody :: T -> Body
_moveBody snake = (heading snake) : (init $ body snake)

instance Arbitrary T where
  arbitrary = do
    location <- arbitrary
    state <- arbitrary
    heading <- arbitrary
    body <- listOf1 arbitrary
    return $ make location state heading body

instance Arbitrary State where
  arbitrary = genericArbitrary uniform
