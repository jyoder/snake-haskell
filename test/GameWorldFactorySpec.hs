module GameWorldFactorySpec
  ( spec_gameWorldFactory
  ) where

import qualified Apple
import qualified GameWorld
import qualified GameWorldFactory
import Geometry
import qualified Snake
import System.Random (mkStdGen)
import Test.Tasty.Hspec

spec_gameWorldFactory :: Spec
spec_gameWorldFactory = do
  describe "makeGameWorld" $ do
    it "returns a new game world with the proper dimensions" $ do
      let gameWorld = GameWorldFactory.makeGameWorld $ mkStdGen 1
       in GameWorld.dimensions gameWorld `shouldBe` (Dimensions 40 40)
    it "returns a new game world with snake in the center heading west" $ do
      let gameWorld = GameWorldFactory.makeGameWorld $ mkStdGen 1
          expSnake = Snake.make (Point 20 20) Snake.Alive West [West, West, West]
       in GameWorld.snake gameWorld `shouldBe` expSnake
    it "returns a new game world with an apple in a psuedo-random location" $ do
      let gameWorld = GameWorldFactory.makeGameWorld $ mkStdGen 1
          expApple = Apple.make (Point 38 31)
       in GameWorld.apple gameWorld `shouldBe` expApple
