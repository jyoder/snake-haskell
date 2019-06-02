module GameWorldSpec
  ( spec_gameWorld
  ) where

import qualified Apple
import qualified GameWorld
import Geometry
import qualified Snake
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

spec_gameWorld :: Spec
spec_gameWorld = do
  describe "make" $ do
    it "returns a game world with the given attributes" $ do
      property $ \dimensions snake apple ->
        let gameWorld = GameWorld.make dimensions snake [apple]
         in GameWorld.dimensions gameWorld == dimensions &&
            GameWorld.rectangle gameWorld == (Rectangle (Point 0 0) dimensions) &&
            GameWorld.snake gameWorld == snake &&
            GameWorld.apple gameWorld == apple
  describe "tick" $ do
    it "advances the snake one unit in the direction it is heading" $ do
      property $ \validGameWorld ->
        let gameWorld = (fromValidGameWorld validGameWorld)
            snake = (GameWorld.snake gameWorld)
            nextGameWorld = (GameWorld.tick gameWorld)
            nextSnake = (GameWorld.snake nextGameWorld)
            nextLocation =
              (adjacentPoint (Snake.heading snake) (Snake.location snake))
         in (Snake.location nextSnake) == nextLocation
    it "grows the snake if it eats an apple" $ do
      property $ \validGameWorld ->
        let gameWorld = (fromValidGameWorld validGameWorld)
            snake = (GameWorld.snake gameWorld)
            appleLocation = (Apple.location (GameWorld.apple gameWorld))
            nextSnakeLocation =
              (adjacentPoint (Snake.heading snake) (Snake.location snake))
            snakeAdjacentToApple = (nextSnakeLocation == appleLocation)
            nextGameWorld = (GameWorld.tick gameWorld)
            snakeLength = (length $ Snake.body snake)
            nextSnakeLength =
              (length (Snake.body $ GameWorld.snake nextGameWorld))
         in if snakeAdjacentToApple
              then nextSnakeLength == snakeLength + 1
              else nextSnakeLength == snakeLength
    it "produces a new apple when the current one is eaten" $ do
      property $ \validGameWorld ->
        let gameWorld = (fromValidGameWorld validGameWorld)
            snake = (GameWorld.snake gameWorld)
            appleLocation = (Apple.location (GameWorld.apple gameWorld))
            nextSnakeLocation =
              (adjacentPoint (Snake.heading snake) (Snake.location snake))
            snakeAdjacentToApple = (nextSnakeLocation == appleLocation)
            nextGameWorld = (GameWorld.tick gameWorld)
            nextAppleLocation = (Apple.location (GameWorld.apple nextGameWorld))
         in if snakeAdjacentToApple
              then nextAppleLocation /= appleLocation
              else nextAppleLocation == appleLocation
    it "kills the snake if it hits a wall" $ do
      let dimensions = (Dimensions 10 10)
          snake = Snake.make (Point 10 5) Snake.Alive East [East, East]
          apple = Apple.make $ Point 1 1
          gameWorld = GameWorld.make dimensions snake [apple]
       in (Snake.state $ GameWorld.snake $ GameWorld.tick gameWorld) `shouldBe`
          Snake.Dead
    it "kills the snake if it eats itself" $ do
      let dimensions = (Dimensions 100 100)
          snake =
            Snake.make
              (Point 50 50)
              Snake.Alive
              West
              [West, West, North, East, South, South, South]
          apple = Apple.make $ Point 1 1
          gameWorld = GameWorld.tick $ GameWorld.make dimensions snake [apple]
       in (Snake.state $ GameWorld.snake $ GameWorld.tick gameWorld) `shouldBe`
          Snake.Dead
    it "does not move the snake if it is dead" $ do
      let dimensions = (Dimensions 10 10)
          snake = Snake.make (Point 0 0) Snake.Alive West [West, West]
          apple = Apple.make $ Point 5 5
          gameWorld = GameWorld.tick $ GameWorld.make dimensions snake [apple]
       in (GameWorld.tick gameWorld) `shouldBe` gameWorld
  describe "turn" $ do
    it "turns the snake in the specified direction" $ do
      let dimensions = (Dimensions 10 10)
          snake = Snake.make (Point 0 0) Snake.Alive West [West, West]
          apple = Apple.make $ Point 5 5
          gameWorld = GameWorld.make dimensions snake [apple]
       in (Snake.heading $ GameWorld.snake $ GameWorld.turn gameWorld North) `shouldBe`
          North
    it "ignores attempts to turn the snake in the exact opposite direction" $ do
      let dimensions = (Dimensions 10 10)
          snake = Snake.make (Point 0 0) Snake.Alive West [West, West]
          apple = Apple.make $ Point 5 5
          gameWorld = GameWorld.make dimensions snake [apple]
       in (Snake.heading $ GameWorld.snake $ GameWorld.turn gameWorld East) `shouldBe`
          West
  describe "show" $ do
    it "returns a string representation of the game world" $ do
      let dimensions = (Dimensions 60 70)
          snake = Snake.make (Point 0 0) Snake.Alive West [West, West]
          apple = Apple.make $ Point 0 0
          gameWorld = GameWorld.make dimensions snake [apple]
       in (show gameWorld) `shouldBe`
          "T {dimensions = Dimensions 60 70, snake = T {location = Point 0 0, state = Alive, heading = West, body = [West,West]}, apples = [T {location = Point 0 0}]}"

maxGameSize :: Int
maxGameSize = 6

newtype ValidGameWorld =
  ValidGameWorld GameWorld.T
  deriving (Show)

instance Arbitrary ValidGameWorld where
  arbitrary = do
    size <- choose (1, maxGameSize)
    snake <- makeSnake size
    apples <- makeApples size
    dimensions <- return $ Dimensions size size
    return $ ValidGameWorld $ GameWorld.make dimensions snake apples

makeSnake :: Int -> Gen Snake.T
makeSnake gameWorldSize = do
  x <- choose (0, gameWorldSize - 1)
  y <- choose (0, gameWorldSize - 1)
  heading <- arbitrary
  size <- choose (1, gameWorldSize)
  body <- listOf1 arbitrary
  return $ Snake.make (Point x y) Snake.Alive heading (take size body)

makeApples :: Int -> Gen [Apple.T]
makeApples gameWorldSize = do
  x <- choose (0, gameWorldSize - 1)
  y <- choose (0, gameWorldSize - 1)
  return [(Apple.make $ Point x y), (Apple.make $ Point (x + 1) (y + 1))]

fromValidGameWorld :: ValidGameWorld -> GameWorld.T
fromValidGameWorld (ValidGameWorld gameWorld) = gameWorld
