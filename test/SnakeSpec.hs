module SnakeSpec
  ( spec_snake
  ) where

import Geometry
import qualified Snake
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

spec_snake :: Spec
spec_snake = do
  describe "make" $ do
    it "returns a snake with the given attributes" $ do
      property $ \location state heading (NonEmpty body) ->
        let snake = Snake.make location state heading body
         in Snake.location snake == location &&
            Snake.state snake == state &&
            Snake.heading snake == heading && Snake.body snake == body
  describe "locations" $ do
    it "returns a list of locations the snake body occupies" $ do
      let snake =
            (Snake.make (Point 1 2) Snake.Alive East [East, South, West, North])
       in Snake.locations snake `shouldBe`
          [(Point 1 2), (Point 0 2), (Point 0 1), (Point 1 1)]
  describe "turn" $ do
    it "changes the snake's heading to the given direction" $ do
      property $ \snake direction ->
        (Snake.heading $ Snake.turn snake direction) == direction
  describe "move" $ do
    it "moves the snake's location one unit in the direction it is heading" $ do
      property $ \snake ->
        let newLocation = Snake.location $ Snake.move snake
         in adjacentPoint (Snake.heading snake) (Snake.location snake) ==
            newLocation
    it "adds a segment to the front of the body in the direction it is heading" $ do
      property $ \snake ->
        Snake.heading snake == (head $ Snake.body $ Snake.move snake)
    it "keeps the length of the snake's body constant" $ do
      property $ \snake ->
        (length (Snake.body snake)) == (length (Snake.body $ Snake.move snake))
  describe "grow" $ do
    it "adds one segment to the snake's body" $ do
      property $ \snake ->
        length (Snake.body (Snake.grow snake)) == length (Snake.body snake) + 1
    it "ensures the last two segments of the tail are equal" $ do
      property $ \snake ->
        let lastTwo = take 2 $ reverse $ Snake.body $ Snake.grow snake
         in lastTwo !! 0 == lastTwo !! 1
  describe "kill" $ do
    it "changes the snake's state to dead" $ do
      property $ \snake -> (Snake.state $ Snake.kill snake) == Snake.Dead
  describe "show" $ do
    it "returns a string representation of the snake" $ do
      (show (Snake.make (Point 1 2) Snake.Alive North [North, North])) `shouldBe`
        "T {location = Point 1 2, state = Alive, heading = North, body = [North,North]}"
