module Trace where

import Codec.Picture

import Trace.Scene
import Trace.Material (toPixel)

render :: (Int, Int) -> Scene -> Image PixelRGBF
render (width, height) scene =
  generateImage
    ( \pixelX pixelY ->
        let x = fromIntegral pixelX / fromIntegral width
            y = fromIntegral pixelY / fromIntegral height
          in toPixel $ trace scene (rayAt x y)
    )
    width
    height
