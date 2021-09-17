module Main where

import Codec.Picture
import Control.Monad (guard)
import qualified Data.Foldable
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Linear hiding (distance, lerp, trace)

import Geometry
import Material
import Surface

data Light = Light
  { position :: Point
  , intensity :: Colour
  }

data Scene = Scene
  { background :: Colour
  , ambient :: Colour
  , objects :: [Object]
  , lights :: [Light]
  }

intersections :: Ray -> Scene -> [(Object, Intersection)]
intersections ray = mapMaybe (\obj -> (obj,) <$> intersect ray obj) . objects

lightAt :: Point -> Direction -> Scene -> Colour
lightAt point normal scene =
  ambient scene + sumV incomingColours
 where
  incomingColours = do
    light <- lights scene

    let toLight = Main.position light - point

    -- We can already check whether the path to this light would have to cross the
    -- immediate surface of the object in question.
    guard (normal `dot` toLight >= 0)

    let direction = normalise toLight
        -- We offset the origin a slight amount to prevent floating point artifacts.
        shadowRay = Ray{origin = point + 0.001 * direction, direction}

    -- Check if we really can see this light from this point
    guard
      (not $ any (\(_, i) -> distance i < norm toLight) $ intersections shadowRay scene)

    pure $ intensity light ^* direction `dot` normal ^/ normSquared toLight

trace :: Scene -> Ray -> Colour
trace scene ray =
  case minimumBy (compare `on` distance . snd) (intersections ray scene) of
    Just (object, intersection) ->
      lightAt (Surface.position intersection) (normal intersection) scene * Material.colour (material object)
    Nothing -> background scene

minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy f xs =
  if null xs
    then Nothing
    else Just $ Data.Foldable.minimumBy f xs

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
    { background = V3 0 0 0
    , objects =
        [ sphere (V3 1.8 (-0.5) 0) 0.8 (Material{colour = red})
        , sphere (V3 1.8 0.3 (-0.5)) 0.5 (Material{colour = blue})
        ]
    , ambient = 0.4
    , lights = [Light{position = V3 1 (-0.5) 1.2, intensity = 0.8}]
    }
 where
  red = V3 1 0 0
  blue = V3 0 0 1

main :: IO ()
main = do
  let (width, height) = (512, 512)
  let image =
        generateImage
          ( \pixelX pixelY ->
              let x = fromIntegral pixelX / fromIntegral width
                  y = fromIntegral pixelY / fromIntegral height
               in toPixel $ trace testScene (rayAt x y)
          )
          width
          height

  savePngImage "test.png" (ImageRGBF image)
