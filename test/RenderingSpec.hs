module RenderingSpec
  ( spec_rendering
  ) where

import qualified Rendering
import Test.Tasty.Hspec

spec_rendering :: Spec
spec_rendering = do
  describe "Action" $ do
    describe "show" $ do
      it "returns a string representation of the action" $ do
        show (Rendering.SetColor Rendering.Red) `shouldBe` "SetColor Red"
  describe "Color" $ do
    describe "show" $ do
      it "returns a string representation of the color" $ do
        show Rendering.Red `shouldBe` "Red"
