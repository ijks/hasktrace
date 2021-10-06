module Trace.Material where

import Codec.Picture (PixelRGBF (..))
import Linear (V3 (..))

import Trace.Geometry

type Colour = N3 Float

toPixel :: Colour -> PixelRGBF
toPixel (V3 r g b) = PixelRGBF r g b

data Material = Material
  { colour :: Colour
  }
