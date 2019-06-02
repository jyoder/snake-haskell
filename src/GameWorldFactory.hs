module GameWorldFactory
  ( makeGameWorld
  ) where

import qualified Apple
import qualified GameWorld
import Geometry
import qualified Snake
import System.Random

gameWorldSize :: Int
gameWorldSize = 40

makeGameWorld :: RandomGen g => g -> GameWorld.T
makeGameWorld g =
  let center = gameWorldSize `div` 2
   in GameWorld.make
        (Dimensions gameWorldSize gameWorldSize)
        (Snake.make (Point center center) Snake.Alive West [West, West, West])
        (_makeApples g)

_makeApples :: RandomGen g => g -> [Apple.T]
_makeApples g = do
  let (gx, gy) = split g
  xs <- return (randomRs (0, gameWorldSize) gx)
  ys <- return (randomRs (0, gameWorldSize) gy)
  map _appleOfPair (zip xs ys)

_appleOfPair :: (Int, Int) -> Apple.T
_appleOfPair pair = Apple.make (Point (fst pair) (snd pair))
