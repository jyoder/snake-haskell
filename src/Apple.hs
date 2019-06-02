module Apple
  ( T
  , make
  , location
  ) where

import GHC.Generics (Generic)
import Generic.Random
import Geometry
import Test.QuickCheck

data T =
  T
    { location :: Point
    }
  deriving (Show, Eq, Generic)

make :: Point -> T
make location = T {location = location}

instance Arbitrary T where
  arbitrary = genericArbitrary uniform
