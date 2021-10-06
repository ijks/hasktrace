module Trace.Light where

import Trace.Geometry
import Trace.Material

data Light = Light
  { position :: Point
  , intensity :: Colour
  }
