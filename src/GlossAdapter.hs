module GlossAdapter
  ( toPicture
  , toDirection
  ) where

import Geometry
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Rendering

toPicture :: Int -> Actions -> Gloss.Picture
toPicture canvasSize actions =
  Gloss.Pictures $ _toPictures $ _colorize Green $ _translate canvasSize actions

toDirection :: Gloss.Event -> Maybe Direction
toDirection (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyUp) Gloss.Down _ _) =
  Just North
toDirection (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Down _ _) =
  Just East
toDirection (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyDown) Gloss.Down _ _) =
  Just South
toDirection (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Down _ _) =
  Just West
toDirection _ = Nothing

_translate :: Int -> Actions -> Actions
_translate canvasSize actions = fmap (_translateAction canvasSize) actions

_translateAction :: Int -> Action -> Action
_translateAction canvasSize (FillRect (Point x y) d) =
  let halfCanvas = canvasSize `div` 2
      x' = x - halfCanvas
      y' = halfCanvas - y
   in (FillRect (Point x' y') d)
_translateAction canvasSize (FillText string (Point x y)) =
  let halfCanvas = canvasSize `div` 2
      x' = x - halfCanvas
      y' = halfCanvas - y
   in (FillText string (Point x' y'))
_translateAction _ action = action

_colorize :: Color -> Actions -> [(Color, Action)]
_colorize _ ((SetColor _):(SetColor c):t) = _colorize c t
_colorize _ ((SetColor c):h:t) = (c, h) : (_colorize c t)
_colorize c (h:t) = (c, h) : (_colorize c t)
_colorize _ [] = []

_toPictures :: [(Color, Action)] -> [Gloss.Picture]
_toPictures ((c, (FillRect p d)):t) = (_toPolygon c p d) : (_toPictures t)
_toPictures ((c, (FillText s p)):t) = (_toText c s p) : (_toPictures t)
_toPictures ((_, (SetColor _)):t) = _toPictures t
_toPictures [] = []

_toPolygon :: Color -> Point -> Dimensions -> Gloss.Picture
_toPolygon color (Point x y) (Dimensions width height) =
  let gColor = _toGlossColor color
      x1 = fromIntegral x
      y1 = fromIntegral y
      x2 = x1 + fromIntegral width
      y2 = y1 - fromIntegral height
   in Gloss.Color
        gColor
        (Gloss.Polygon [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)])

_toText :: Color -> String -> Point -> Gloss.Picture
_toText color string (Point x y) =
  Gloss.Color
    (_toGlossColor color)
    (Gloss.Translate (fromIntegral x) (fromIntegral y) $
     Gloss.Scale 0.2 0.2 $ Gloss.Text string)

_toGlossColor :: Color -> Gloss.Color
_toGlossColor Green = Gloss.green
_toGlossColor Red = Gloss.red
_toGlossColor Black = Gloss.black
