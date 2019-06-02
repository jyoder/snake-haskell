module Main where

import qualified GameRuntime
import qualified GameWorldFactory
import System.Random

main :: IO ()
main = do
    g <- getStdGen
    GameRuntime.start $ GameWorldFactory.makeGameWorld g
