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
  , surfaces :: [SomeSurface]
  }

trace :: Scene -> Ray -> Colour
trace scene ray =
  let intersections =
        surfaces scene
          & mapMaybe (intersect ray)
   in if null intersections
        then background scene
        else
          let intersection = minimumBy (compare `on` distance) intersections
           in min 1 (distance intersection * 0.4)

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
    { background = 0.0
    , surfaces =
        [ Surface $ Sphere{center = V3 1.8 (-0.5) 0, radius = 0.8}
        , Surface $ Sphere{center = V3 1.8 0.5 0, radius = 0.8}
        ]
    }

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

  savePngImage "test.png" (ImageYF image)
