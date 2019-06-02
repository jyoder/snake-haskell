module GeometrySpec
  ( spec_geometry
  ) where

import Geometry
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

spec_geometry :: Spec
spec_geometry = do
  describe "oppositeDirection" $ do
    it "returns the opposite of the given direction" $ do
      property $ \direction ->
        _shouldBeOpposites direction $ oppositeDirection direction
  describe "withinDimensions" $ do
    it "returns whether the point is within the dimensions (inclusive)" $ do
      property $ \point rectangle ->
        (withinRectangle point rectangle) == (_withinRectangle point rectangle)
  describe "adjacentPoint" $ do
    it "returns the point adjacent in the given direction" $ do
      property $ \direction point ->
        _shouldBeAdjacent direction point $ adjacentPoint direction point
  describe "show" $ do
    it "returns a string representation of a point" $ do
      (show (Point 2 3)) `shouldBe` "Point 2 3"
    it "returns a string representation of dimensions" $ do
      (show (Dimensions 4 5)) `shouldBe` "Dimensions 4 5"
    it "returns a string representation of dimensions" $ do
      (show (Rectangle (Point 1 2) (Dimensions 4 5))) `shouldBe`
        "Rectangle (Point 1 2) (Dimensions 4 5)"
    it "returns a string representation of a direction" $ do
      (show East) `shouldBe` "East"

_shouldBeOpposites :: Direction -> Direction -> Bool
_shouldBeOpposites North South = True
_shouldBeOpposites South North = True
_shouldBeOpposites East West = True
_shouldBeOpposites West East = True
_shouldBeOpposites _ _ = False

_withinRectangle :: Point -> Rectangle -> Bool
_withinRectangle (Point x1 y1) (Rectangle (Point x2 y2) (Dimensions width height)) =
  x1 >= x2 && x1 <= x2 + width && y1 >= y2 && y1 <= y2 + height

_shouldBeAdjacent :: Direction -> Point -> Point -> Bool
_shouldBeAdjacent North (Point _ y1) (Point _ y2) = y1 - y2 == 1
_shouldBeAdjacent East (Point x1 _) (Point x2 _) = x2 - x1 == 1
_shouldBeAdjacent South (Point _ y1) (Point _ y2) = y2 - y1 == 1
_shouldBeAdjacent West (Point x1 _) (Point x2 _) = x1 - x2 == 1
