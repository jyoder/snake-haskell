module AppleSpec
  ( spec_apple
  ) where

import qualified Apple
import Geometry
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

spec_apple :: Spec
spec_apple = do
  describe "make" $ do
    it "returns an apple with the given attributes" $ do
      property $ \location ->
        let apple = Apple.make location
         in (Apple.location apple) == location
  describe "show" $ do
    it "returns a string representation of the apple" $ do
      (show (Apple.make $ Point 1 2)) `shouldBe` "T {location = Point 1 2}"
