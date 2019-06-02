module Geometry
  ( Point(..)
  , Dimensions(..)
  , Rectangle(..)
  , Direction(..)
  , oppositeDirection
  , withinRectangle
  , adjacentPoint
  ) where

import GHC.Generics (Generic)
import Generic.Random
import Test.QuickCheck

data Point =
  Point Int Int
  deriving (Show, Eq, Generic)

data Dimensions =
  Dimensions Int Int
  deriving (Show, Eq, Generic)

data Rectangle =
  Rectangle Point Dimensions
  deriving (Show, Eq)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Generic)

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection South = North
oppositeDirection East = West
oppositeDirection West = East

withinRectangle :: Point -> Rectangle -> Bool
withinRectangle (Point x1 y1) (Rectangle (Point x2 y2) (Dimensions width height)) =
  x1 >= x2 && x1 <= x2 + width && y1 >= y2 && y1 <= y2 + height

adjacentPoint :: Direction -> Point -> Point
adjacentPoint North (Point x y) = Point x (y - 1)
adjacentPoint East (Point x y) = Point (x + 1) y
adjacentPoint South (Point x y) = Point x (y + 1)
adjacentPoint West (Point x y) = Point (x - 1) y

instance Arbitrary Point where
  arbitrary = genericArbitrary uniform

instance Arbitrary Rectangle where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    width <- arbitrarySizedNatural
    height <- arbitrarySizedNatural
    return $ Rectangle (Point x y) (Dimensions width height)

instance Arbitrary Dimensions where
  arbitrary = genericArbitrary uniform

instance Arbitrary Direction where
  arbitrary = genericArbitrary uniform
