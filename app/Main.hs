module Main where

import Codec.Picture
import qualified Data.Vector as Vector
import Linear hiding (trace)

import Trace.Light
import Trace.Material
import Trace.Scene
import Trace.Surface

testScene :: Scene
testScene =
  Scene
    { background = V3 0 0 0
    , objects =
        Vector.fromList
          [ sphere (V3 1.8 (-0.5) 0) 0.8 (Material{colour = red})
          , sphere (V3 1.8 0.3 (-0.5)) 0.5 (Material{colour = blue})
          ]
    , ambient = 0.4
    , lights = Vector.fromList [Light{position = V3 1 (-0.5) 1.2, intensity = 0.8}]
    }
 where
  red = V3 1 0 0
  blue = V3 0 0 1

largeScene :: Scene
largeScene =
  Scene
    { background = V3 0 0 0
    , objects =
        Vector.fromList
          [ sphere (V3 (x + 3) y z) 0.1 (Material{colour = 1})
          | (x, y, z) <- (,,) <$> range <*> range <*> range
          ]
    , ambient = 0.2
    , lights =
        Vector.fromList
          [ Light{position = V3 2 (-1) 0.75, intensity = V3 1 0 0}
          , Light{position = V3 2 1 0.75, intensity = V3 0 1 0}
          , Light{position = V3 2 0 (-0.75), intensity = V3 0 0 1}
          ]
    }
 where
  range = [-2, -1.5 .. 2]

main :: IO ()
main = do
  let (width, height) = (512, 512)
  let scene = largeScene
  let image =
        generateImage
          ( \pixelX pixelY ->
              let x = fromIntegral pixelX / fromIntegral width
                  y = fromIntegral pixelY / fromIntegral height
               in toPixel $ trace scene (rayAt x y)
          )
          width
          height

  savePngImage "test.png" (ImageRGBF image)
