module Material where

import Codec.Picture (PixelRGBF)

type Colour = PixelRGBF

data Material = Material
  { colour :: Colour
  }
