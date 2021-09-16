{-# LANGUAGE RankNTypes #-}

module Surface where

import Linear

import Geometry
import Material

data Intersection = Intersection
  { position :: Point
  , normal :: Direction
  , distance :: Scalar
  -- probably also surface colour or the like
  }
  deriving (Show)

class Surface a where
  intersect :: Ray -> a -> Maybe Intersection

data Sphere = Sphere
  { center :: Point
  , radius :: Scalar
  }
  deriving (Show)

instance Surface Sphere where
  intersect ray sphere = do
    let toCenter = center sphere - origin ray
        length = toCenter `dot` direction ray
        radiusSquared = radius sphere ^ 2
        perpendicular = toCenter - length *^ direction ray
        inside = if normSquared toCenter < radiusSquared then 1 else (-1)
        distance = length + inside * sqrt (radiusSquared - normSquared perpendicular)
        position = ray `at` distance
    -- This looks like a lot of computation up front, but we don't actually
    -- do all of it all of the time, because of laziness.

    if normSquared perpendicular > radiusSquared || distance <= 0
      then Nothing
      else
        pure $
          Intersection
            { position
            , distance
            , normal = negate inside *^ normalise (position - center sphere)
            }

data SomeSurface where
  Surface :: Surface a => a -> SomeSurface

instance Surface SomeSurface where
  intersect ray (Surface s) = intersect ray s

data Object = Object
  { surface :: SomeSurface
  , material :: Material
  }

instance Surface Object where
  intersect ray = intersect ray . surface

sphere :: Point -> Scalar -> Material -> Object
sphere center radius material =
  Object{surface = Surface $ Sphere{center, radius}, material}
