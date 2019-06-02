module GameRendererSpec
  ( spec_gameRenderer
  ) where

import qualified Apple
import qualified GameRenderer
import qualified GameWorld
import Geometry
import Rendering
import qualified Snake
import Test.Tasty.Hspec

spec_gameRenderer :: Spec
spec_gameRenderer = do
  describe "render" $ do
    it "renders the apple and snake during normal gameplay" $ do
      let apple = Apple.make $ Point 2 8
          snake = Snake.make (Point 20 30) Snake.Alive West [West, West]
          appleActions =
            [SetColor Red, FillRect (Point 30 120) (Dimensions 15 15)]
          snakeActions =
            [ SetColor Green
            , FillRect (Point 300 450) (Dimensions 15 15)
            , FillRect (Point 315 450) (Dimensions 15 15)
            ]
          dimensions = (Dimensions 60 70)
          gameWorld = GameWorld.make dimensions snake [apple]
       in GameRenderer.render (Dimensions 600 600) gameWorld `shouldBe`
          concat [appleActions, snakeActions]
    it "renders an end game message when the snake dies" $ do
      let apple = Apple.make $ Point 2 8
          snake = Snake.make (Point 20 30) Snake.Dead West [West, West]
          appleActions =
            [SetColor Red, FillRect (Point 30 120) (Dimensions 15 15)]
          snakeActions =
            [ SetColor Green
            , FillRect (Point 300 450) (Dimensions 15 15)
            , FillRect (Point 315 450) (Dimensions 15 15)
            ]
          messageActions =
            [SetColor Black, FillText "You have died." (Point 190 300)]
          dimensions = (Dimensions 60 70)
          gameWorld = GameWorld.make dimensions snake [apple]
       in GameRenderer.render (Dimensions 600 600) gameWorld `shouldBe`
          concat [appleActions, snakeActions, messageActions]
