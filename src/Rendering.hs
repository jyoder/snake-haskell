module Rendering
  ( Actions
  , Action(..)
  , Color(..)
  ) where

import Geometry

type Actions = [Action]

data Action
  = SetColor Color
  | FillRect Point Dimensions
  | FillText String Point
  deriving (Show, Eq)

data Color
  = Green
  | Red
  | Black
  deriving (Show, Eq)
