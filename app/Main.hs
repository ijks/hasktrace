module Main where

import Codec.Picture
import Control.Applicative (liftA2)
import Data.Foldable (minimumBy)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import Linear hiding (distance, lerp, trace)

import Geometry
import Material
import Surface

data Scene = Scene
  { background :: Colour
  , objects :: [Object]
  }

trace :: Scene -> Ray -> Colour
trace scene ray =
  let intersections =
        objects scene
          & mapMaybe (\o -> (o,) <$> intersect ray o)
   in if null intersections
        then background scene
        else
          let (object, _) = minimumBy (compare `on` distance . snd) intersections
           in colour $ material object

-- intersections & minimumBy (compare `on` (.distance)) & (.distance)

-- TODO: camera record
rayAt :: Scalar -> Scalar -> Ray
rayAt screenX screenY =
  let depth = 1.0
      y = lerp screenX (-1) 1
      z = lerp screenY 1 (-1)
      rayTarget = V3 depth y z
   in aimedAt 0 rayTarget

testScene :: Scene
testScene =
  Scene
    { background = PixelRGBF 0 0 0
    , objects =
        [ sphere (V3 1.8 (-0.5) 0) 0.8 (Material{colour = red})
        , sphere (V3 1.8 0.5 0) 0.8 (Material{colour = blue})
        ]
    }
 where
  red = PixelRGBF 1 0 0
  blue = PixelRGBF 0 0 1

main :: IO ()
main = do
  let (width, height) = (512, 512)
  let image =
        generateImage
          ( \pixelX pixelY ->
              let x = fromIntegral pixelX / fromIntegral width
                  y = fromIntegral pixelY / fromIntegral height
               in trace testScene (rayAt x y)
          )
          width
          height

  savePngImage "test.png" (ImageRGBF image)
