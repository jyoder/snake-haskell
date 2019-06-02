module GameRuntime
  ( start
  ) where

import qualified GameRenderer
import qualified GameWorld
import Geometry
import qualified GlossAdapter
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Rendering

size :: Int
size = 600

render :: GameWorld.T -> Rendering.Actions
render = GameRenderer.render (Dimensions size size)

tickFrequency :: Int
tickFrequency = 10

start :: GameWorld.T -> IO ()
start gameWorld =
  Gloss.play
    (Gloss.InWindow "Snake" (size, size) (0, 0))
    Gloss.white
    tickFrequency
    gameWorld
    (\world -> GlossAdapter.toPicture size (render world))
    (\event world -> processInput world event)
    (\_ world -> GameWorld.tick world)

processInput :: GameWorld.T -> Gloss.Event -> GameWorld.T
processInput gameWorld event =
  case GlossAdapter.toDirection event of
    Just direction -> GameWorld.turn gameWorld direction
    Nothing -> gameWorld
