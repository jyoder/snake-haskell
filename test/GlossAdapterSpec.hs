module GlossAdapterSpec
  ( spec_glossAdapter
  ) where

import Geometry
import qualified GlossAdapter
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Rendering
import Test.Tasty.Hspec

spec_glossAdapter :: Spec
spec_glossAdapter = do
  describe "toPicture" $ do
    it "translates coordinates of actions into the gloss coordinate system" $ do
      shouldBe
        (GlossAdapter.toPicture
           100
           [SetColor Red, FillRect (Point 0 0) (Dimensions 10 10)])
        (Gloss.Pictures
           [ Gloss.Color (Gloss.makeColor 1.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-50.0, 50.0)
               , (-40.0, 50.0)
               , (-40.0, 40.0)
               , (-50.0, 40.0)
               , (-50.0, 50.0)
               ]
           ])
    it "sets the appropriate color for each resulting polygon" $ do
      shouldBe
        (GlossAdapter.toPicture
           100
           [ SetColor Red
           , FillRect (Point 0 0) (Dimensions 10 10)
           , FillRect (Point 1 1) (Dimensions 10 10)
           , SetColor Green
           , FillRect (Point 2 2) (Dimensions 10 10)
           , SetColor Black
           , FillRect (Point 3 3) (Dimensions 10 10)
           ])
        (Gloss.Pictures
           [ Gloss.Color (Gloss.makeColor 1.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-50.0, 50.0)
               , (-40.0, 50.0)
               , (-40.0, 40.0)
               , (-50.0, 40.0)
               , (-50.0, 50.0)
               ]
           , Gloss.Color (Gloss.makeColor 1.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-49.0, 49.0)
               , (-39.0, 49.0)
               , (-39.0, 39.0)
               , (-49.0, 39.0)
               , (-49.0, 49.0)
               ]
           , Gloss.Color (Gloss.makeColor 0.0 1.0 0.0 1.0) $
             Gloss.Polygon
               [ (-48.0, 48.0)
               , (-38.0, 48.0)
               , (-38.0, 38.0)
               , (-48.0, 38.0)
               , (-48.0, 48.0)
               ]
           , Gloss.Color (Gloss.makeColor 0.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-47.0, 47.0)
               , (-37.0, 47.0)
               , (-37.0, 37.0)
               , (-47.0, 37.0)
               , (-47.0, 47.0)
               ]
           ])
    it "sets the appropriate color for each resulting polygon" $ do
      shouldBe
        (GlossAdapter.toPicture
           100
           [ SetColor Red
           , FillRect (Point 0 0) (Dimensions 10 10)
           , FillRect (Point 1 1) (Dimensions 10 10)
           , FillRect (Point 2 2) (Dimensions 10 10)
           ])
        (Gloss.Pictures
           [ Gloss.Color (Gloss.makeColor 1.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-50.0, 50.0)
               , (-40.0, 50.0)
               , (-40.0, 40.0)
               , (-50.0, 40.0)
               , (-50.0, 50.0)
               ]
           , Gloss.Color (Gloss.makeColor 1.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-49.0, 49.0)
               , (-39.0, 49.0)
               , (-39.0, 39.0)
               , (-49.0, 39.0)
               , (-49.0, 49.0)
               ]
           , Gloss.Color (Gloss.makeColor 1.0 0.0 0.0 1.0) $
             Gloss.Polygon
               [ (-48.0, 48.0)
               , (-38.0, 48.0)
               , (-38.0, 38.0)
               , (-48.0, 38.0)
               , (-48.0, 48.0)
               ]
           ])
    it "adapts actions to draw text" $ do
      shouldBe
        (GlossAdapter.toPicture 100 [FillText "Hi mom." (Point 10 10)])
        (Gloss.Pictures
           [ Gloss.Color (Gloss.makeColor 0.0 1.0 0.0 1.0) $
             (Gloss.Translate
                (-40.0)
                40.0
                (Gloss.Scale 0.2 0.2 (Gloss.Text "Hi mom.")))
           ])
    it "uses green as the default color" $ do
      shouldBe
        (GlossAdapter.toPicture 100 [FillRect (Point 0 0) (Dimensions 10 10)])
        (Gloss.Pictures
           [ Gloss.Color (Gloss.makeColor 0.0 1.0 0.0 1.0) $
             Gloss.Polygon
               [ (-50.0, 50.0)
               , (-40.0, 50.0)
               , (-40.0, 40.0)
               , (-50.0, 40.0)
               , (-50.0, 50.0)
               ]
           ])
    it "uses the latter color when two colors are given consecutively" $ do
      shouldBe
        (GlossAdapter.toPicture
           100
           [ SetColor Red
           , SetColor Green
           , FillRect (Point 0 0) (Dimensions 10 10)
           ])
        (Gloss.Pictures
           [ Gloss.Color (Gloss.makeColor 0.0 1.0 0.0 1.0) $
             Gloss.Polygon
               [ (-50.0, 50.0)
               , (-40.0, 50.0)
               , (-40.0, 40.0)
               , (-50.0, 40.0)
               , (-50.0, 50.0)
               ]
           ])
    it "returns an empty list when a color but no rectangle is given" $ do
      shouldBe (GlossAdapter.toPicture 100 [SetColor Red]) (Gloss.Pictures [])
    describe "toDirection" $ do
      it "returns north when the up key is given" $ do
        GlossAdapter.toDirection
          (Gloss.EventKey
             (Gloss.SpecialKey Gloss.KeyUp)
             Gloss.Down
             Gloss.Modifiers
               { Gloss.shift = Gloss.Up
               , Gloss.ctrl = Gloss.Up
               , Gloss.alt = Gloss.Up
               }
             (0.0, 0.0)) `shouldBe`
          (Just North)
      it "returns east when the right key is given" $ do
        GlossAdapter.toDirection
          (Gloss.EventKey
             (Gloss.SpecialKey Gloss.KeyRight)
             Gloss.Down
             Gloss.Modifiers
               { Gloss.shift = Gloss.Up
               , Gloss.ctrl = Gloss.Up
               , Gloss.alt = Gloss.Up
               }
             (0.0, 0.0)) `shouldBe`
          (Just East)
      it "returns south when the down key is given" $ do
        GlossAdapter.toDirection
          (Gloss.EventKey
             (Gloss.SpecialKey Gloss.KeyDown)
             Gloss.Down
             Gloss.Modifiers
               { Gloss.shift = Gloss.Up
               , Gloss.ctrl = Gloss.Up
               , Gloss.alt = Gloss.Up
               }
             (0.0, 0.0)) `shouldBe`
          (Just South)
      it "returns west when the left key is given" $ do
        GlossAdapter.toDirection
          (Gloss.EventKey
             (Gloss.SpecialKey Gloss.KeyLeft)
             Gloss.Down
             Gloss.Modifiers
               { Gloss.shift = Gloss.Up
               , Gloss.ctrl = Gloss.Up
               , Gloss.alt = Gloss.Up
               }
             (0.0, 0.0)) `shouldBe`
          (Just West)
      it "returns nothing when a non-direction key is given" $ do
        GlossAdapter.toDirection
          (Gloss.EventKey
             (Gloss.SpecialKey Gloss.KeyF1)
             Gloss.Down
             Gloss.Modifiers
               { Gloss.shift = Gloss.Up
               , Gloss.ctrl = Gloss.Up
               , Gloss.alt = Gloss.Up
               }
             (0.0, 0.0)) `shouldBe`
          Nothing
