module Trace.Scene where

import Control.Monad (guard)
import qualified Data.Foldable
import Data.Function (on)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Linear hiding (distance, lerp, trace)

import Trace.Geometry
import Trace.Light
import Trace.Material
import Trace.Surface

data Scene = Scene
  { background :: !Colour
  , ambient :: !Colour
  , objects :: Vector Object
  , lights :: Vector Light
  }

intersections :: Ray -> Scene -> Vector (Object, Intersection)
intersections ray = Vector.mapMaybe (\obj -> (obj,) <$> intersect ray obj) . objects

lightAt :: Point -> Direction -> Scene -> Colour
lightAt point normal scene =
  ambient scene + sumV incomingColours
 where
  incomingColours = do
    light <- lights scene

    let toLight = Trace.Light.position light - point

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
      lightAt (Trace.Surface.position intersection) (normal intersection) scene
        * Trace.Material.colour (material object)
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
